#!/usr/bin/env ruby
#--
#
# Copyright (c) 2010 Ian Langworth
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#
#++

# BIG DISCLAIMER: I wrote this in 2005 when I wanted to learn Ruby. It's
# probably very un-Rubyish, but the results of using it are fun.

require 'rmagick'
require 'pathname'
require 'pp'  # For debugging.
require 'set'
require 'yaml'

module Spittoon

  @@config = nil

  def Spittoon.load_config(filepath)
    yaml = File.new(filepath).read
    @@config = YAML::load(yaml)
    @@basedir = Pathname.new(filepath).dirname
  end

  def Spittoon.initialized?
    return !@@config.nil?
  end

  def Spittoon.path_to(param)
    return @@basedir + @@config[param]
  end

  def Spittoon.get(param)
    if @@config.key?(param)
      return @@config[param]
    else
      raise "Configuration parameter `#{param}' required but not defined"
    end
  end

  def Spittoon.has?(param)
    return @@config.key?(param)
  end

  def Spittoon.generate_comic(spec)
    strip = Strip.new

    for bitspec in spec['chat']
      # Keys in the spec must become Symbols.
      for key in bitspec.keys
        bitspec[key.to_sym] = bitspec[key]
      end
      strip.add_bit(bitspec)
    end

    strip.extrapolate_static_chars!
    strip.extrapolate_placement!

    image = StripRenderer.new(strip).render
    if Spittoon.has?('comment')
      image['comment'] = Spittoon.get('comment')
    end
    return image
  end

  class Spec

    def initialize(content)
      begin

        raise #XXX
        @spec = YAML::load(content)

      rescue

        all_characters = Set.new(Spittoon.get('characters'))
        available_characters = Set.new(Spittoon.get('characters'))
        character_aliases = Hash.new

        chat = []
        for line in content.split(/\n/)

          next if line.empty? or line.match(/^\s*#/)
          break if line.match(/^----*\s*/)

          match = line.match(/
              ^
              (\w+)
              (->\w+)?
              (\S)?
              \s*
              (.*?)
              ( \s* \( [^\)]+ \) )?
              $
              /x)
          next unless match
          name, to, actionchar, text, mods = match.captures

          text = '' if text.nil?
          to.sub!(/^->/, '') unless to.nil?

          alias_charname = proc do |given|
            if not all_characters.member?(given)
              if character_aliases.has_key?(given)
                character_aliases[given]
              elsif available_characters.empty?
                raise "No more characters available for '#{given}': #{line}"
              else
                old = given
                result = available_characters.to_a.random_element
                available_characters.delete(result)
                character_aliases[old] = result
                puts "Aliasing #{old} to #{result}"
                result
              end
            end
          end

          name = alias_charname.call(name)
          to = alias_charname.call(to) if to

          raise "Invalid line in input: #{line}" if name.nil?

          face, pose = nil, nil
          if mods
            mods.gsub!(/[^\w\/]/, '')
            face, pose = mods.split(/\//)
          end

          action = text.empty? ? 'watches' : 'says'
          action = 'emotes' if actionchar == '*'

          chat << {'name' => name,
                   'to' => to,
                   'action' => action,
                   'face' => face,
                   'pose' => pose,
                   'text' => text}
        end
        raise "No lines in chat" if chat.empty?
        @spec = {'chat' => chat}
      end
    end

    def [](item)
        if(@spec == nil)
          return []
        end
        return @spec[item]
    end

  end

  class UnplacableBalloonError < RuntimeError; end

  class StripContentError < RuntimeError; end

  class BalloonWontFitError < RuntimeError
    attr_accessor :panel, :bit

    def initialize(args)
      @panel = args[:panel]
      @bit   = args[:bit]
    end
  end

  class Strip
    attr_reader :title, :panels

    def initialize(title='unknown')
      @title = title
      @panels = []
    end

    def to_s
      @panels.map {|p| p.to_s}.join("----\n")
    end

    def last_panel
      @panels.last
    end

    def add_bit(args)
      @panels << Panel.new if
        @panels.empty? or
        self.last_panel.full? or
        self.last_panel.contains_char?(args[:name])
      bit = Bit.new(args)
      self.last_panel << bit
      self.last_panel.index = @panels.length
      return bit
    end

    def recalculate_panel_indexes!
      @panels.each_with_index do |panel, i|
        panel.index = i
      end
    end

    def extrapolate_static_chars!
      @panels.each do |panel|
        if ! panel.full?
          possibles = panel.referents.reject do |a|
            panel.contains_char?(a)
          end
          possibles.each do |referent|
            panel << Bit.new(:name => referent) unless panel.full?
          end
        end
      end
      self
    end

    def extrapolate_placement!
      previous = nil

      @panels = @panels.map do |old|
        new = Panel.new
        new.index = old.index
        old.bits.each do |bit|
          variants = {}
          new.generate_variants_using(bit).each do |v|
            variants[ v.penalty(previous) ] = v
          end
          previous = new = variants[ variants.keys.min ]
        end
        new
      end
      self.recalculate_panel_indexes!

      self
    end

    def render(*args)
      tries = 0
      snip = nil
      begin
        snip = StripRenderer.new(self).render(*args)
      rescue UnplacableBalloonError
        tries += 1
        if tries > 3
          raise "can't render strip #{self}"
        else
          GC::start
          retry
        end
      end
      return snip
    end

  end

  class Panel
    attr_accessor :bits, :index

    def initialize(bits=[])
      @bits = bits
      @index = -1
    end

    def to_s
      bits = @bits.map {|b| b.to_s}
      bitlist = bits.empty? ? '' : "\n\t" + bits.join(",\n\t") + "\n\t";
      return "<Panel index=#{index} bits=[#{bitlist}]>"
    end

    def ==(other)
      self.bits == other.bits
    end

    def <<(bit)
      raise "panel already contains bit name #{bit.name}" if
        self.contains_char?(bit.name)
      @bits << bit
    end

    def is_facing?(x, y)
      x_position = position_of_bit(x)
      y_position = position_of_bit(y)
      return ( ( x_position < y_position and x.facing == :right ) or
               ( x_position > y_position and x.facing == :left ) )
    end

    def generate_variants_using(bit)
      variants = []
      [:left, :right].each do |direction|
        combinations_with(bit).each do |combination|
          v = Panel.new(combination)
          v.index = self.index
          v.set_facing_of_char(bit.name, direction)
          variants << v
        end
      end
      variants
    end

    def penalty(previous)
      sum = 0
      @bits.each do |a|
        @bits.reject {|b| a == b}.each do |b|
          sum += facing_penalty(a, b) +
            neighbors_penalty(previous, a)
        end
      end
      sum
    end

    def facing_penalty(a, b)
      penalty = 0

      a_is_not_facing_b = ! is_facing?(a,b)
      b_is_not_facing_a = ! is_facing?(b,a)

      if a.to.nil?
        penalty += 4 if a_is_not_facing_b
        penalty += 2 if b_is_not_facing_a
      elsif a.to == b.name
        penalty +=  4 if b_is_not_facing_a
        penalty += 40 if a_is_not_facing_b
        penalty +=  4 * distance_between_bits(a, b)
      end

      return penalty
    end

    def neighbors_penalty(previous, bit)
      return 0 if previous.nil?
      penalty = 0

      this_index = self.position_of_bit(bit)
      prev_index = previous.position_of_bit(bit)
      return 0 if prev_index.nil?

      [-1, 1].each do |difference|
        this_neighbor = self.bits[ this_index + difference ]
        prev_neighbor = self.bits[ prev_index + difference ]
        penalty += 1 if not this_neighbor.nil? and
                        not prev_neighbor.nil? and
                        this_neighbor != prev_neighbor
      end

      penalty
    end

    def distance_between_bits(a, b)
      a_index = position_of_bit(a)
      b_index = position_of_bit(b)
      raise "bit a not found in panel" if a_index.nil?
      raise "bit b not found in panel" if b_index.nil?
      ( (a_index-b_index).abs - 1 )
    end

    def set_facing_of_char(name, direction)
      bit = bit_for(name) or
        raise "couldn't find bit for #{name}"
      bit.facing = direction
    end

    # Generate placement combinations using the given bit.
    # For example, if the current bits were:
    #   [ 'Sally', 'Suzy' ]
    # And you asked for +combinations_of('Fred')+, this would
    # return three tuples:
    #   [ 'Fred', 'Sally', 'Suzy' ],
    #   [ 'Sally', 'Fred', 'Suzy' ],
    #   [ 'Sally', 'Fred', 'Suzy' ]
    def combinations_with(bit)
      (0..self.size).collect do |i|
          self.copy_of_bits.insert(i, bit.dup)
      end
    end

    def position_of_bit(bit)
      @bits.index(bit)
    end

    def bit_for(name)
      raise "need a name" if name.nil?
      @bits.detect {|b| b.name == name}
    end

    def copy_of_bits
      @bits.collect {|b| b.dup}
    end

    def chars
      @bits.collect {|b| b.name}
    end

    def size
      self.chars.length
    end

    def empty?
      self.size == 0
    end

    def full?
      self.size >= Spittoon.get('max_chars_in_panel')
    end

    def contains_char?(char)
      self.chars.include?(char)
    end

    def referents
      @bits.collect {|b| b.to}.compact
    end

    def render(*args)
      PanelRenderer.new(self).render(*args)
    end
  end

  class Bit
    attr_accessor :name, :action, :to, :text, :facing, :face, :pose

    def initialize(args)
      @name = args[:name]
      if not Spittoon.get('characters').include?(@name)
        raise "Name '#{name}' is not a valid character name"
      end

      @action = args[:action] || (args[:text] ? 'says' : 'watches')
      @to = args[:to]
      @text = args[:text]
      @facing = args[:facing]
      @face = args[:face]
      @pose = args[:pose]
    end

    def ==(other)
      self.name     == other.name &&
        self.action == other.action &&
        self.to     == other.to &&
        self.text   == other.text &&
        self.facing == other.facing
    end

    def inspect
      "[#{self.class}#0x#{self.object_id}: #{self.to_s}]"
    end

    def to_s
      attrs = []
      for method in [:name, :facing, :action, :to, :face, :pose, :text]
        value = self.__send__(method)
        value = '' if value.nil?
        value = value.gsub(/\W+/, '')[0..20] if method == :text
        attrs << "#{method}=#{value}"
      end
      return "<Bit #{attrs.join(' ')}>"
    end

    def render(*args)
      BitRenderer.new(self).render(*args)
    end

  end

  class StripRenderer

    attr_accessor :strip

    def initialize(strip)
      @strip = strip
    end

    def random_background
      return Dir.glob("#{Spittoon.get('background_dir')}/*.{jpg,png,gif}").random_element
    end

    def random_style
      return rand(2) == 0 ? :open : :closed
    end

    def render_canvas
      p = @strip.panels
      w = Spittoon.get('strip_width')
      hue = rand(100)/100.0
      bg = random_background

      # XXX - why doesn't the border appear without a montage?
      case @strip.panels.length
      when 1
        size = w * 0.60
        image = p[0].render(:width => size,
                            :height => size,
                            :style => :closed,
                            :hue => hue,
                            :bg => bg)
        list = Magick::ImageList.new
        list << image
        montage = list.montage do
          self.background_color = Spittoon.get('strip_background_color')
          self.geometry = "#{size}x#{size}+5"
        end
        return montage.first
      when 2
        buffer = w * 0.02 # 20px for 600px width
        size = (w * 4/9 - buffer) + (w * 1/8)
        list = Magick::ImageList.new
        p.each_with_index do |panel, i|
          style = (i == 1) ? random_style : :closed
          list << panel.render(:width => size,
                               :height => size,
                               :style => style,
                               :hue => hue,
                               :bg => bg)
        end
        montage = list.montage do
          self.background_color = Spittoon.get('strip_background_color')
          self.geometry = "#{size}x#{size}+5"
        end
        return montage.first
      when 3
        # New! Render a 3-panel strip as 1-by-3
        buffer = w * 0.02 # 20px for 600px width
        size = (w * 1/3 - buffer) + (w * 1/8)
        size = Spittoon.get('strip_width') * 0.6
        list = Magick::ImageList.new
        p.each_with_index do |panel, i|
          style = (i == 1) ? random_style : :closed
          list << panel.render(:width => size,
                               :height => size,
                               :style => style,
                               :hue => hue,
                               :bg => bg)
        end
        montage = list.montage do
          self.background_color = Spittoon.get('strip_background_color')
          self.geometry = "#{size}x#{size}+5+5"
          self.tile = "1x3"
        end
        return montage.first
      else
        actual_cell_size = Spittoon.get('strip_width') * 0.6
        rendered_cell_size = Spittoon.get('strip_width') * 0.6
        size = Spittoon.get('strip_width') / 4
        list = Magick::ImageList.new
        strip.panels.each_with_index do |panel, i|
          style = if (i > 0 and i < strip.panels.length - 1)
                    random_style
                  else
                    :closed
                  end
          list << panel.render(:width => rendered_cell_size,
                               :height => rendered_cell_size,
                               :style => style,
                               :hue => hue,
                               :bg => bg)
        end
        return list.montage do
          self.background_color = Spittoon.get('strip_background_color')
          self.geometry = "#{actual_cell_size}x#{actual_cell_size}+5+5"
        end.first
      end
    end

    def render
      attempt = 0
      begin
        puts "Rendering strip, attempt #{attempt+1} with #{@strip.panels.length} panels"
        raise UnplacableBalloonError, "Text will never fit" if attempt > 3
        canvas = render_canvas.trim
        final = canvas

        if Spittoon.has?('caption')
          title = Magick::Image.read('caption:' + Spittoon.get('caption')) do
            self.size = "#{canvas.columns}x"
            if Spittoon.has?('title_font_file')
              self.font = Spittoon.get('title_font_file')
            end
            self.pointsize = Spittoon.get('title_font_size')
            self.background_color = Spittoon.get('strip_background_color')
            self.fill = Spittoon.get('title_font_color')
            self.gravity = Magick::EastGravity
          end.first
          final = Magick::Image.new(canvas.columns, canvas.rows + title.rows) do
            self.background_color = Spittoon.get('strip_background_color')
          end
          final = final.paste(canvas).paste(title, 0, canvas.rows)
        end

        if Spittoon.has?('strip_padding')
          padding = Spittoon.get('strip_padding')
          padded = Magick::Image.new(final.columns + padding * 2,
                                     final.rows + padding * 2) do
            self.background_color = Spittoon.get('strip_background_color')
          end
          final = padded.paste(final, padding, padding)
        end

        puts "Done."
        return final

      rescue BalloonWontFitError => e
        bits        = e.panel.bits
        bit_index   = bits.index(e.bit)
        panel_index = @strip.panels.index(e.panel)

        if bits.length == 1
          raise UnplacableBalloonError, "This bit will never fit in a panel: #{e.bit}"
        end

        first, second = bits.chop_at( bit_index ).map {|b| Panel.new(b)}
        @strip.panels.replace_index(panel_index, first, second)
        strip.extrapolate_placement!

        puts "Trouble placing panel: #{e.panel}"
        attempt += 1
        retry
      ensure
        GC::start
      end
    end

  end

  class PanelRenderer

    attr_accessor :panel

    def initialize(panel)
      @panel = panel
    end

    def bit_snips
      return @panel.bits.map {|b| b.render}
    end

    def get_balloons
      return @panel.bits.map {|b| Balloon.new(b)}
    end

    def render(given={})
      puts "Rendering panel #{@panel.index+1}"
      options = {
        :width    => Spittoon.get('panel_size'),
        :height   => Spittoon.get('panel_size'),
        :shape    => :box,
        :gravity  => Magick::CenterGravity,
        :hue      => 0.0,
        :bg       => 'granite:',
        :style    => :closed,
      }.merge(given)

      frame_size = [options[:width], options[:height]].min
      wiggle_room = frame_size * 0.02
      vertical_wiggle = rand(wiggle_room) - wiggle_room
      vertical_middle = frame_size / 2 + wiggle_room
      max_balloon_height = (vertical_middle * 9.5 / 10.0).to_i

      snip = Magick::Image.new(frame_size, frame_size) do
        self.background_color = 'transparent'
      end

      snip, centers = apply_chars(snip, bit_snips(), frame_size,
                                  vertical_middle)
      snip = snip.tint_from_hue(options[:hue], 0.5 + rand(4)/10.0)

      # The 3rd and 4th arguments here can be played with to adjust cramped
      # balloons and bad tail placement when space in a panel is limited.
      balloons = create_balloons(snip, frame_size, max_balloon_height,
                            vertical_middle - wiggle_room,
                            centers, options[:style])
      snip = snip.paste(balloons)

      final = generate_background(options[:width], options[:height],
                                  options[:bg], options[:style])
      final = final.tint_from_hue(options[:hue], 0.2 + rand(3)/10.0)

      if Spittoon.get('shadows')
        final = final.paste(snip.shadow(3, 3, 3.0, 0.3))
      end
      final = final.paste(snip)

      apply_finishing!(final, options[:style])

      return final
    end

    def generate_background(width, height, path, style)
      workspace = Magick::Image.new(width, height)
      bg = Magick::Image.read(path).first.modulate(1.0, 0.0, 1.0)

      # Resize the background -- but not too small!
      bg_reduction_factor =
        case self.panel.size
        when 1 then (75 + rand(15))
        when 2 then (60 + rand(20))
        when 3 then (50 + rand(20))
        else (50 + rand(10))
        end
      bg_reduction_factor /= 100
      scale = [width.to_f / bg.rows.to_f,
               height.to_f / bg.columns.to_f,
               bg_reduction_factor].max
      bg.scale!(scale)

      # Figure out where we're going to place it
      bg.crop!(rand(width - bg.columns), rand(width - bg.rows), width, height)

      # Draw "closed" or "open" panels
      case style
      when :closed
        workspace = workspace.paste(bg, 0, 0)
      when :open
        # I learned this from: # http://xrl.us/rmagickslides (Link to schf.uc.org)
        overlay = Magick::Image.new(width, height) do
          self.background_color = 'transparent'
        end
        stop_y = height - (height / 3)
        mask_fill = Magick::GradientFill.new(0, 0, width, 0, '#000', '#fff')
        mask = Magick::Image.new(width, height - stop_y, mask_fill)
        bg.matte = true
        mask.matte = false
        bg.composite!(mask, 0, stop_y, Magick::CopyOpacityCompositeOp)
        workspace.composite!(bg, 0, 0, Magick::OverCompositeOp)
      else
        raise "unsupported style: #{style}"
      end
      return workspace
    end

    def apply_chars(bg, bit_snips, width, at_height)
      margin = width *
        case self.panel.size
        when 1 then (62 + rand(18))/100.0
        when 2 then (20 + rand(25))/100.0
        when 3 then (5 + rand(30))/100.0
        else    0.00
        end

      total_bits_width = bit_snips.inject(0) do |total, snip|
        total + snip.columns
      end
      usable_space = width - margin
      char_reduction_factor = usable_space.to_f / total_bits_width.to_f

      l = ( margin / 2 ).to_i
      centers = []
      bit_snips.each_with_index do |snip, i|
        reduced = snip.scale(char_reduction_factor)
        bg = bg.paste(reduced, l, at_height)
        centers[i] = l + reduced.columns/2
        l += reduced.columns
      end

      return bg, centers
    end

    def max_allowable(a, b)
      if a.center < b.center
        return [ a.channel.begin + Spittoon.get('balloon_padding')*3, a.center ].max .. b.channel.end
      else
        return b.channel.begin .. [ a.channel.end - Spittoon.get('balloon_padding')*3, a.center ].min
      end
    end

    def reduce_channel(a, b)
      if a.center < b.center
        a.channel.begin .. [ a.channel.end, b.channel.begin ].min
      else
        [ a.channel.begin, b.channel.end ].max .. a.channel.end
      end
    end

    def create_balloons(snip, width, max_balloon, max_tail, centers, style)
      snip = Magick::Image.new(snip.columns, snip.rows) do
        self.background_color = 'transparent'
      end

      # modify the centers of the balloons
      balloons = get_balloons()
      balloons.each_with_index {|b,i| b.center = centers[i]}

      # calculate balloons' routing channels and horizontal placement
      balloons.each_with_index do |a, i|
        next unless a.placable

        # set initial width and routing channel
        a.squeeze_into!(width - Spittoon.get('balloon_padding')*2 - 10, max_balloon)
        a.channel = [(a.center - a.snip.columns), 0].max ..
                    [(a.center + a.snip.columns), width].min

        # resize channel so that it at least has a tail
        0.upto(i - 1) do |j|
          b = balloons[j]
          next unless b.placable
          a.channel = max_allowable(b, a)
        end

        # if balloon width is smaller than channel, place the balloon in
        # the channel randomly, weighted toward the left. otherwise, fail.
        # XXX it should be weighted toward the character's position
        if a.channel_width > a.snip_width
          wiggle_room = a.channel_width - a.snip_width
          wiggle_room *= 0.66
          wiggle = (0 .. wiggle_room).random_integer_near_middle
          a.channel = (a.channel.begin + wiggle) .. (a.channel.end + wiggle)
        elsif not a.squeeze_into!(a.channel_width, max_balloon)
          raise BalloonWontFitError, {
            :panel => @panel,
            :bit => balloons[i-1].bit,
          }
        end
      end

      # calculate each balloon's vertical placement
      previous = []
      balloons.select {|current| current.placable}.each do |current|

        # XXX - this used to be... previous.inject(Spittoon.get('balloon_padding'))
        current.placed_at_y = previous.inject(0) do |memo, prev|
          # if we can fit at the same height, do so
          if prev.right_edge < current.left_edge
            memo
          # we read left-to-right, so don't place later balloons at the same
          # height as balloons that have already been placed and are to the
          # right
          elsif prev.left_edge > current.right_edge
            [ memo, prev.top_edge + 15 ].max
          # otherwise, there simply isn't room on this height
          else
            [ memo, prev.bottom_edge ].max
          end
        end

        if current.bottom_edge > max_balloon
          raise BalloonWontFitError, { :panel => @panel, :bit => current.bit }
        end

        previous << current
      end

      # if the balloons don't take all of the vertical space, slide them
      # vertically toward the middle or top-middle, with a little extra random
      # wiggle room
      begin
        balloons.select {|b| b.placable}.each do |b|
          wiggle = max_balloon - b.bottom_edge
          if previous.last and previous.last != b
            wiggle = (max_balloon - previous.last.bottom_edge) / 2.0
          end
          b.placed_at_y += (0 .. wiggle).random_integer_near_middle
        end
      rescue NoMethodError => e
        # do nothing
      end

      # reduce routing channels, place each tail
      balloons.each_with_index do |a, i|
        next unless a.placable
        a.channel = a.left_edge .. a.right_edge
        0.upto(i-1) do |j|
          b = balloons[j]
          next unless b.placable
          b.channel = reduce_channel(b, a)
        end
      end

      # draw each balloon (outline, contents, tail)
      balloons.select {|a| a.placable}.each do |a|

        start_x = a.channel.random_integer_near_middle

        # our tail end isn't the real center, otherwise curves don't
        # seem to point to the middle of the character's face
        end_x = start_x + (a.center-start_x)/2

        control_x = start_x + (end_x-start_x)/4
        control_y = a.bottom_edge + (max_tail - a.bottom_edge)/2

        l = Magick::Draw.new
        l.stroke('black')
        l.stroke_width(Spittoon.get('balloon_stroke'))

        # convenience variables for tweakable constants
        k = Spittoon.get('balloon_bulge')
        w = Spittoon.get('tail_width')

        # multiple Q's here are for readability
        case a.bit.action
        when 'says'
          case style
          when :closed
            l.fill('white')
            l.path("M#{start_x-w} #{a.bottom_edge+k}" +
                   "Q#{a.left_edge+(start_x-a.left_edge)/2} #{a.bottom_edge+k/2}" +
                   " #{a.left_edge} #{a.bottom_edge}" +
                   "Q#{a.left_edge-k} #{a.top_edge+(a.bottom_edge-a.top_edge)/2}" +
                   " #{a.left_edge} #{a.top_edge}" +
                   "Q#{a.left_edge+(a.right_edge-a.left_edge)/2} #{a.top_edge-k}" +
                   " #{a.right_edge} #{a.top_edge}" +
                   "Q#{a.right_edge+k} #{a.top_edge+(a.bottom_edge-a.top_edge)/2}" +
                   " #{a.right_edge} #{a.bottom_edge}" +
                   "Q#{a.right_edge-(a.right_edge-start_x)/2} #{a.bottom_edge+k/2}" +
                   " #{start_x+w} #{a.bottom_edge+k}" +
                   "Q#{control_x} #{control_y} #{end_x} #{max_tail}" +
                   "Q#{control_x} #{control_y} #{start_x-w} #{a.bottom_edge+k}")
          when :open
            l.fill_opacity(0)
            l.path("M#{start_x} #{a.bottom_edge}" +
                  "Q#{control_x} #{control_y} #{end_x} #{max_tail}")
          end
        when 'emotes'
          l.fill('white')
          l.path("L#{a.left_edge} #{a.bottom_edge}" +
                 "L#{a.left_edge} #{a.top_edge}" +
                 "L#{a.right_edge} #{a.top_edge}" +
                 "L#{a.right_edge} #{a.bottom_edge}" +
                 "L#{a.left_edge} #{a.bottom_edge}")
        end

        l.draw(snip)
        snip = snip.paste(a.snip, a.channel.begin, a.placed_at_y)
      end

      return snip
    end

    # this method mutates snip because there's no functional version of
    # Magick::Draw.draw that returns a new image
    def apply_finishing!(snip, style)
      case style
      when :closed
        snip.border!(4, 4, 'black')
      else
        snip
      end
    end

  end

  class Balloon
    attr_accessor :center, :bit, :channel, :snip, :placable, :placed_at_y

    def initialize(bit)
      @bit = bit
      @placable = (@bit.action == 'says' or @bit.action == 'emotes')
    end

    def to_s
      if @channel
        return "<Balloon #{left_edge},#{top_edge} to #{right_edge},#{bottom_edge}>"
      else
        return "<Balloon>"
      end
    end

    def channel_width
      return @channel.width
    end

    def snip_width
      return @snip.columns
    end

    def snip_height
      return @snip.rows
    end

    def left_edge
      return @channel.begin
    end

    def right_edge
      return self.left_edge + @snip.columns
    end

    def top_edge
      return @placed_at_y || 0
    end

    def bottom_edge
      return self.top_edge + @snip.rows
    end

    def squeeze_into!(max_width, max_height)
      return false if max_width < Spittoon.get('min_balloon_width')
      text = Spittoon.get('chat_uppercase') ? bit.text.upcase : bit.text
      max_width -= Spittoon.get('balloon_padding') * 1;

      @snip = Magick::Image.read("caption:#{text}") do
        self.size = "#{max_width-2}x"  # -2 to here because some letters were cut off
        self.antialias = true
        self.background_color = 'transparent'
        if Spittoon.has?('chat_font_file')
          self.font = Spittoon.get('chat_font_file')
        end
        self.pointsize = Spittoon.get('chat_font_size')
        self.fill = 'black'
        self.gravity = Magick::CenterGravity
      end
      @snip = @snip.first.trim.border(Spittoon.get('balloon_padding'), Spittoon.get('balloon_padding'), 'transparent')
      return @snip.rows <= max_height
    end

  end

  class BitRenderer

    attr_accessor :bit

    def initialize(bit)
      @bit = bit
    end

    def render(add_halo=true)
      puts "Rendering bit #{@bit}"
      face = Magick::Image.read(self.choose_face).first
      body = Magick::Image.read(self.choose_body).first

      # XXX - painfully specific here
      case @bit.name
        when 'easy'   then face.scale!(0.8)
        when 'bucket' then face.scale!(0.9)
        when 'melon'  then face.scale!(0.9)
      end
      head_multiplier = Spittoon.get('body_on_head_distance')
      case @bit.name
        when 'melon' then  head_multiplier = head_multiplier - 0.1
      end

      width = [face.columns, body.columns].max
      height = body.rows + face.rows * head_multiplier
      snip = Magick::Image.new(width, height) do
        self.background_color = 'transparent'
      end

      face_width_offset = face.columns == width ? 0 : (width - face.columns)/2
      body_width_offset = body.columns == width ? 0 : (width - body.columns)/2

      final = snip \
        .paste(body, body_width_offset, face.rows * head_multiplier) \
        .paste(face, face_width_offset, 0)

      final = final.halo(Spittoon.get('halo_size')) if add_halo

      return self.bit.facing == :left ? final.flop : final
    end

    def choose_item_from_sets(sets, name, sync)
      if !name.nil? and !name.empty?
        # An item has been provided.
        if sets.key?(name)
          # It's a set. Pick one in the set.
          return sets[name].random_element
        else
          # It's specific. Pick that one.
          return name
        end
      elsif sync
        # No item. Do some logic to figure out which set we should use.
        if @bit.action == 'says'
          return sets['active'].random_element
        else
          return sets['passive'].random_element
        end
      else
        return sets['passive'].random_element
      end
    end

    def try_to_choose_item(basedir, sets, item, sync)
      path = nil
      tries = 0
      while true
        item = choose_item_from_sets(sets, item, sync)
        path = basedir + "#{@bit.name}-#{item}.png"
        if not File.exists?(path)
          tries += 1
          raise "Couldn't find file '#{path}' for bit #{@bit}" if tries > 10
        else
          break
        end
      end
      return path
    end

    def choose_face
      basedir = Spittoon.path_to('face_artwork_dir')
      sets = Spittoon.get('face_sets')
      return try_to_choose_item(basedir, sets, @bit.face, true)
    end

    def choose_body
      basedir = Spittoon.path_to('pose_artwork_dir')
      sets = Spittoon.get('pose_sets')
      return try_to_choose_item(basedir, sets, @bit.pose, false)
    end

  end

end

module Magick

  class Image

    def paste(image, x=0, y=0)
      return self.composite(image, x, y, OverCompositeOp)
    end

    def halo(size)
      bg = Magick::Image.new(self.columns + size * 2,
                              self.rows + size * 2)
      return bg \
        .paste(self, size, size) \
        .blur_image(size/2, size) \
        .matte_replace(0, 0) \
        .composite(bg, 0, 0, InCompositeOp) \
        .paste(self, size, size) \
        .trim
    end

    def tint_from_hue(hue, intensity=1.0)
      pixel = Pixel.from_hsla(hue, 1.0, 0.5)
      return self.tint(pixel, intensity)
    end

  end
end

class Array

  def chop_at(index)
    return self[ 0 .. index-1 ], self[ index .. self.length-1 ]
  end

  def replace_index(index, *with)
    self.delete_at(index)
    self.insert(index, *with)
  end

  def random_element
    return self[ rand(self.length) ]
  end

end

class Range

  def random_integer_near_middle
    wiggle_room = self.width * 0.5
    wiggle = rand(wiggle_room) - wiggle_room/2
    return (self.begin + self.width/2 + wiggle).to_i
  end

  def width
    return self.end - self.begin
  end

end
