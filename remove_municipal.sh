cat city_province.txt | sed 's/\([a-z]*\),municipal/\1,\1/g' | sed 's/\([a-z]*\),autonomous/\1,\1/g'
