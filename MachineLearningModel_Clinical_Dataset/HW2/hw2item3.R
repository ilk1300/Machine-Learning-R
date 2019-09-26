#Question3

#create a function to calculate scoring matrix
#remark: for ith base and jth base in two sequences, the matrix index is [i+1,j+1]
calculate_scoring_matrix <- function(seq1,seq2) {
  row_num <- length(seq1)+1 #row and column length is sequence length plus 1
  col_num <- length(seq2)+1
  scoring_matrix <- matrix(numeric(0), nrow = row_num, ncol = col_num)
  scoring_matrix[1,1] <- 0 #top left corner in the matrix is 0
  for (n in 2:row_num) { #for column one, parse from 2nd row, since matrix[1,1]=0
    scoring_matrix[n,1] <- scoring_matrix[n-1,1]-8 #parse each cell in 1st column and insert gap score
  }
  for (m in 2:col_num) { #same with parsing columns in 1st row
    scoring_matrix[1,m] <- scoring_matrix[1,m-1]-8
  }
  for (i in 1:length(seq1)) {  #parsing the matrix row by row
    for (j in 1:length(seq2)) { #parsing each column in that row
      vertical_gap_score <- scoring_matrix[i,j+1]-8 
      horizontal_gap_score <- scoring_matrix[i+1,j]-8
      if (seq1[i] == seq2[j]) {
        match_score <- scoring_matrix[i,j]+5
        final_score <- max(match_score, vertical_gap_score, horizontal_gap_score)
      } #get highest score from vertical, horizontal and diagonal
      else {
        mismatch_score <- scoring_matrix[i,j]-4
        final_score <- max(mismatch_score, vertical_gap_score, horizontal_gap_score)
      } #get highest score from vertical, horizontal and diagonal
      scoring_matrix[i+1,j+1] <- final_score #assign final score to cell
    }
  }
  return(scoring_matrix)
}

#import dna sequences and build vectors of nucleotide bases
mouse_hexa='gctgctggaaggggagctggccggtgggccatggccggctgcaggctctgggtttcgctgctgctggcggcggcgttggcttgcttggccacggcactgt'
human_hexa='acgtgattcgccgataagtcacgggggcgccgctcacctgaccagggtctcacgtggccagccccctccgagaggggagaccagcgggccatgacaagct'
#split dna sequence by each base
mouse_bases <- strsplit(mouse_hexa, split=NULL)[[1]]
human_bases <- strsplit(human_hexa, split=NULL)[[1]]

matrix_result <- calculate_scoring_matrix(human_bases,mouse_bases)

#create a function to do traceback
traceback_alignment <- function(matrix_result, seq1, seq2) {
  alignment_seq1=c() #create empty string to hold alignment
  alignment_seq2=c()
  i <- length(seq1)
  j <- length(seq2)
  while (i>0 && j>0) { #strart traceback from bottom right of the matrix
    #if score in current cell come from diagonal score +5 (but needs to be a match) or -4
    if((matrix_result[i+1,j+1] == matrix_result[i,j]+5) && seq1[i] == seq2[j] | (matrix_result[i+1,j+1] == matrix_result[i,j]-4)) {
      alignment_seq1 <- c(alignment_seq1, seq1[i]) #add 2 bases to alignment
      alignment_seq2 <- c(alignment_seq2, seq2[j])
      i <- i-1 #go up one row and go left one column
      j <- j-1
    }
    #if horizontal score equal to vertical score
    #check sequence length and insert gap in shorter sequence
    else if(matrix_result[i,j+1] == matrix_result[i+1,j]) {
      if(length(alignment_seq1) >= length(alignment_seq2)) {
        alignment_seq1 <- c(alignment_seq1, seq1[i])
        alignment_seq2 <- c(alignment_seq2, '-')
      }
    }
    #if traceback vertical, insert gap in top sequence
    else if(matrix_result[i+1,j+1] == matrix_result[i,j+1]-8) {
      alignment_seq1 <- c(alignment_seq1, seq1[i])
      alignment_seq2 <- c(alignment_seq2, '-')
      i <- i-1
    }
    #if traceback horizontal, insert gap in left sequence
    else {
      alignment_seq1 <- c(alignment_seq1, '-')
      alignment_seq2 <- c(alignment_seq2, seq2[j])
      j <- j-1
    }
  }
  #reverse the order to two strings and combime them into a matrix
  alignment_seq1 <- rev(alignment_seq1)
  alignment_seq2 <- rev(alignment_seq2)
  alignment_result <- rbind(alignment_seq1, alignment_seq2)
  return(alignment_result)
}

#print final alignment
print(traceback_alignment(matrix_result, human_bases, mouse_bases))
