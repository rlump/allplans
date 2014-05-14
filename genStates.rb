require "csv"

CSV.foreach("states.csv", encoding: "iso-8859-1:UTF-8") do |row|

  puts "#{row[0].gsub(/\./,'')},#{row[6].gsub(/\,/,'')}"


end
