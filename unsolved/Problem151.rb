N = 10000000
sum = (0..N).map {
  sheets = [1]
  times = 0
  (0..15).each do |batch|
    if sheets.length == 1 && batch != 0 && batch != 15
      times += 1
    end

    while true
      i = rand(sheets.length)
      sheet = sheets[i]
      sheets.delete_at(i)
      if sheet == 5
        break
      else
        sheets += [sheet + 1, sheet + 1]
      end
    end
  end

  times
}

puts sum.reduce(:+) * 1.0 / N
