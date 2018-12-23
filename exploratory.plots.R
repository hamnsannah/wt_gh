#exploratory plots

dept.plot <- ggplot(data = sales.by.dept) +
  geom_segment(aes(x=1, xend=2, y=`2016`, yend=`2017`)) + 
  geom_text(aes(label=Department), y=`2016`, x=1)