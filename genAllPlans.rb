require "csv"

CSV.foreach("allplans.csv", encoding: "iso-8859-1:UTF-8") do |row|

  puts "#{row[0].gsub(/\,/,' ')},#{row[1].gsub(/\,/,' ')},#{row[2].gsub(/\,/,' ')},#{row[3].gsub(/\,/,'')},#{row[24] ? row[24].gsub(/\$|\,/,''):""}"


end
