module UsageHelper
    def UsageHelper::usage( exit_code )

        File::open( $0, 'r').readlines.each_with_index do | line, idx |
            next if idx == 0
            if( line =~ /^#/ )
                puts line.gsub(/^#\ ?/,'')
            else
                puts #RDoc adds extra line so we do too
                exit( exit_code )
            end
        end
    end
end