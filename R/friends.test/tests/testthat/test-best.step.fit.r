test_that("best.step.fit returns expected values for known input", {
  ranks <- c(1, 97, 98, 99, 100)
  tags.no <- 100
  result <- best.step.fit(ranks, tags.no)
  expect_true(result$best.step.rank %in% seq(1,96))
  #split into 1 and all others
  expect_equal(result$population.on.left, 1)
  expect_equal(result$collections.on.left, c(1))
  expect_equal(result$collections.on.right, c(2, 3, 4, 5))
})


#ranks <- c(1,1,1,1,6,6,6,6,6,6)
test_that("best.step.fit the same value as independent code", {
  ranks <- c(1,2,3,4,5,6,7,8)
  tag.lim <- 21 #tag.no
  col.order <- order(ranks)
  ranks <- ranks[col.order]
  l.lim <- 0:tag.lim #limit of enumeration of l
  k <- length(ranks)
  lkl <- rep(-1000, tag.lim) #likelihoods
  step.pos <- rep(0, tag.lim) #no of ranks on left
  pos <- 1
  for (l in l.lim){
    q <- which( ranks <= l)
    if (length(q) == 0 ){
      #print('trololo\n')
    }else if (length(q) < length(ranks)){
      m <- max(q)
      p <- m/k
      lkl[pos] <- m * log(p/l) + (k - m) * log ((1-p )/(tag.lim-l))
      step.pos[pos] <- length(q)
      pos <- pos + 1
    } else if (length(q) == length(ranks)){
      break;
    }
  }
  stp.pos <- min(which(lkl == max(lkl)))
  cols_no<-step.pos[stp.pos] #collections before jump
  #print(tgs_no)
  bsf<-best.step.fit(ranks, tag.lim) #best.friends
  #print(bsf)
  expect_equal(bsf$population.on.left,cols_no)
})

