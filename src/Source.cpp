#include <RcppArmadillo.h>
using namespace Rcpp;

#include<ctime>
using namespace std;


#include <thread>
#include <chrono>

// [[Rcpp::depends(RcppArmadillo)]]



// [[Rcpp::export]]
arma::vec getWins(arma::vec PO) {
  arma::vec wins(6); 
  wins[0] = PO[0]+PO[1]+PO[2]+PO[3]+PO[4]+PO[5]+PO[6]+PO[7]+PO[8]+PO[9]+PO[10]+PO[11]+PO[12]+PO[13]+PO[14];
  wins[1] = PO[15]+PO[16]+PO[17]+PO[18]+PO[19]+PO[20]+PO[21]+PO[22]+PO[23]+PO[24]+PO[25]+PO[26]-PO[0]+1-PO[1]+1-PO[2]+1;
  wins[2] = PO[27]+PO[28]+PO[29]+PO[30]+PO[31]+PO[32]+PO[33]+PO[34]+PO[35]-PO[3]+1-PO[4]+1-PO[5]+1-PO[15]+1-PO[16]+1-PO[17]+1;
  wins[3] = PO[36]+PO[37]+PO[38]+PO[39]+PO[40]+PO[41]-PO[6]+1-PO[7]+1-PO[8]+1-PO[18]+1-PO[19]+1-PO[20]+1-PO[27]+1-PO[28]+1-PO[29]+1;
  wins[4] = PO[42]+PO[43]+PO[44]-PO[9]+1-PO[10]+1-PO[11]+1-PO[21]+1-PO[22]+1-PO[23]+1-PO[30]+1-PO[31]+1-PO[32]+1-PO[36]+1-PO[37]+1-PO[38]+1;
  wins[5] = 0-PO[12]+1-PO[13]+1-PO[14]+1-PO[24]+1-PO[25]+1-PO[26]+1-PO[33]+1-PO[34]+1-PO[35]+1-PO[39]+1-PO[40]+1-PO[41]+1-PO[42]+1-PO[43]+1-PO[44]+1;
  return wins;
}

// [[Rcpp::export]]
arma::vec getWins2(arma::vec PO) {
  arma::vec wins(6);
  wins[0] = PO[0]+PO[1]+PO[2]+PO[3]+PO[4];
  wins[1] = 3-PO[0]+PO[5]+PO[6]+PO[7]+PO[8];
  wins[2] = 3-PO[1]+3-PO[5]+PO[9]+PO[10]+PO[11];
  wins[3] = 3-PO[2]+3-PO[6]+3-PO[9]+PO[12]+PO[13];
  wins[4] = 3-PO[3]+3-PO[7]+3-PO[10]+3-PO[12]+PO[14];
  wins[5] = 3-PO[4]+3-PO[8]+3-PO[11]+3-PO[13]+3-PO[14];
  return wins;
}


// [[Rcpp::export]]
arma::vec getSets(arma::vec PO) {
  arma::vec sets(15); 
  for (int i = 0; i < 15; i++) {
    if (PO[3*i] + PO[3*i+1] + PO[3*i+2] >= 2) {
      sets[i] = 1;
    }
  }
  return sets;
}

// [[Rcpp::export]]
arma::vec getSets2(arma::vec PO) {
  arma::vec sets(15); 
  for (int i = 0; i < 15; i++) {
    if (PO[i] >= 2) {
      sets[i] = 1;
    }
  }
  return sets;
}

// [[Rcpp::export]]
arma::vec getSetWins(arma::vec PO) {
  arma::vec sets = getSets(PO); 
  arma::vec setWins(6);
  setWins[0] = sets[0]+sets[1]+sets[2]+sets[3]+sets[4];
  setWins[1] = sets[5]+sets[6]+sets[7]+sets[8]-sets[0]+1;
  setWins[2] = sets[9]+sets[10]+sets[11]-sets[1]+1-sets[5]+1;
  setWins[3] = sets[12]+sets[13]-sets[2]+1-sets[6]+1-sets[9]+1;
  setWins[4] = sets[14]-sets[3]+1-sets[7]+1-sets[10]+1-sets[12]+1;
  setWins[5] = 0-sets[4]+1-sets[8]+1-sets[11]+1-sets[13]+1-sets[14]+1;
  return setWins;
}

// [[Rcpp::export]]
arma::vec getSetWins2(arma::vec PO) {
  arma::vec sets = getSets2(PO); 
  arma::vec setWins(6);
  setWins[0] = sets[0]+sets[1]+sets[2]+sets[3]+sets[4];
  setWins[1] = sets[5]+sets[6]+sets[7]+sets[8]-sets[0]+1;
  setWins[2] = sets[9]+sets[10]+sets[11]-sets[1]+1-sets[5]+1;
  setWins[3] = sets[12]+sets[13]-sets[2]+1-sets[6]+1-sets[9]+1;
  setWins[4] = sets[14]-sets[3]+1-sets[7]+1-sets[10]+1-sets[12]+1;
  setWins[5] = 0-sets[4]+1-sets[8]+1-sets[11]+1-sets[13]+1-sets[14]+1;
  return setWins;
}


// [[Rcpp::export]]
arma::vec getOPScores(arma::mat Sets, arma::vec Wins) {
  arma::mat sets(15, 2);
  sets.col(0) = {1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  sets.col(1) = {2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  
  arma::vec OPScore(6);
  for (int i = 0; i < 6; i++) {
    arma::uvec loc = find(sets.col(0) == i+1);
    for (int j = 0; j < size(loc)[0]; j++) {
      if (Sets[loc[j]] == 1) {
        OPScore[i] = OPScore[i] + 2*Wins[sets(loc[j], 1)-1] - 15;
      }
    }
    loc = find(sets.col(1) == i+1);
    for (int j = 0; j < size(loc)[0]; j++) {
      if (Sets[loc[j]] == 0) {
        OPScore[i] = OPScore[i] + 2*Wins[sets(loc[j], 0)-1] - 15;
      }
    }
  }
  return OPScore;
}


// [[Rcpp::export]]
arma::vec getStandings(arma::vec PO) {
  arma::mat Standings(6, 7);
  Standings.col(0) = {1, 2, 3, 4, 5, 6};
  arma::vec Wins = getWins(PO);
  Standings.col(1) = Wins;
  Standings.col(2) = getSetWins(PO);
  Standings = Standings.rows(sort_index(Standings.col(2), "descend"));
  Standings = Standings.rows(sort_index(Standings.col(1), "descend"));
  
  // calculate standings
  Standings(0, 3) = 1;
  for (int i = 1; i < 6; i++) {
    if (Standings(i, 1) == Standings(i-1, 1) && Standings(i, 2) == Standings(i-1, 2)) {
      Standings(i, 3) = Standings(i-1, 3);
    } else {
      Standings(i, 3) = i+1;
    }
  }
  

  arma::mat sets(15, 2);
  sets.col(0) = {1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  sets.col(1) = {2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  
  arma::mat schedule(45, 2);
  schedule.col(0) = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5};
  schedule.col(1) = {2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 4, 4, 4, 5, 5, 5, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6};

  int nunique = size(find_unique(Standings.col(3)))[0];
  arma::mat Sets = getSets(PO);
  if (nunique != 6) {
    for (int i = 0; i < 6; i++) {
      arma::uvec players = find(Standings.col(3) == i + 1);
      
      if (size(players)[0] >= 2) {
        arma::vec tiedplayers = Standings(arma::span(min(players), max(players)), 0);
        arma::vec H2H(size(tiedplayers)[0]);
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          for (int k = 0; k < size(tiedplayers)[0]; k++) {
            if (tiedplayers[j] < tiedplayers[k]) {
              arma::uvec loc = find(sets.col(0) == tiedplayers[j] && sets.col(1) == tiedplayers[k]);
              if (Sets(loc[0], 0) == 0) {
                H2H[k]++;
              } else {
                H2H[j]++;
              }
            }
          }
        }
        
        arma::vec H2HW(size(tiedplayers)[0]);
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          for (int k = 0; k < size(tiedplayers)[0]; k++) {
            if (tiedplayers[j] < tiedplayers[k]) {
              arma::uvec loc = find(schedule.col(0) == tiedplayers[j] && schedule.col(1) == tiedplayers[k]);
              for (int l = 0; l < size(loc)[0]; l++) {
                if (PO(loc[l], 0) == 0) {
                  H2HW[k]++;
                } else {
                  H2HW[j]++;
                }
              }
            }
          }
        }
        
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          arma::uvec loc = find(Standings.col(0) == tiedplayers[j]);
          Standings(loc[0], 4) = H2H[j];
          Standings(loc[0], 5) = H2HW[j];
        }
      }
    }
    Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
    Standings.col(6) = getOPScores(Sets, Wins);
    Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
    Standings = Standings.rows(sort_index(Standings.col(6), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(5), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(4), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(2), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(1), "descend"));
    Standings.col(3) = {1, 2, 3, 4, 5, 6};
  }
  Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
  return Standings.col(3);
}

// [[Rcpp::export]]
arma::vec getStandings2(arma::vec PO) {
  arma::mat Standings(6, 7);
  Standings.col(0) = {1, 2, 3, 4, 5, 6};
  arma::vec Wins = getWins2(PO);
  Standings.col(1) = Wins;
  Standings.col(2) = getSetWins2(PO);
  Standings = Standings.rows(sort_index(Standings.col(2), "descend"));
  Standings = Standings.rows(sort_index(Standings.col(1), "descend"));
  
  // calculate standings
  Standings(0, 3) = 1;
  for (int i = 1; i < 6; i++) {
    if (Standings(i, 1) == Standings(i-1, 1) && Standings(i, 2) == Standings(i-1, 2)) {
      Standings(i, 3) = Standings(i-1, 3);
    } else {
      Standings(i, 3) = i+1;
    }
  }
  
  
  arma::mat sets(15, 2);
  sets.col(0) = {1, 1, 1, 1, 1, 2, 2, 2, 2, 3, 3, 3, 4, 4, 5};
  sets.col(1) = {2, 3, 4, 5, 6, 3, 4, 5, 6, 4, 5, 6, 5, 6, 6};
  
  arma::mat schedule(45, 2);
  schedule.col(0) = {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 3, 3, 4, 4, 4, 4, 4, 4, 5, 5, 5};
  schedule.col(1) = {2, 2, 2, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 3, 3, 3, 4, 4, 4, 5, 5, 5, 6, 6, 6, 4, 4, 4, 5, 5, 5, 6, 6, 6, 5, 5, 5, 6, 6, 6, 6, 6, 6};
  
  int nunique = size(find_unique(Standings.col(3)))[0];
  arma::mat Sets = getSets2(PO);
  if (nunique != 6) {
    for (int i = 0; i < 6; i++) {
      arma::uvec players = find(Standings.col(3) == i + 1);
      
      if (size(players)[0] >= 2) {
        arma::vec tiedplayers = Standings(arma::span(min(players), max(players)), 0);
        arma::vec H2H(size(tiedplayers)[0]);
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          for (int k = 0; k < size(tiedplayers)[0]; k++) {
            if (tiedplayers[j] < tiedplayers[k]) {
              arma::uvec loc = find(sets.col(0) == tiedplayers[j] && sets.col(1) == tiedplayers[k]);
              if (Sets(loc[0], 0) == 0) {
                H2H[k]++;
              } else {
                H2H[j]++;
              }
            }
          }
        }
        
        arma::vec H2HW(size(tiedplayers)[0]);
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          for (int k = 0; k < size(tiedplayers)[0]; k++) {
            if (tiedplayers[j] < tiedplayers[k]) {
              arma::uvec loc = find(sets.col(0) == tiedplayers[j] && sets.col(1) == tiedplayers[k]);
              // for (int l = 0; l < size(loc)[0]; l++) {
              //   if (PO(loc[l], 0) == 0) {
              //     H2HW[k]++;
              //   } else {
              //     H2HW[j]++;
              //   }
              // }
              H2HW[j] = H2HW[j] + PO(loc[0], 0);
              H2HW[k] = H2HW[k] + 3 - PO(loc[0], 0);
            }
          }
        }
        
        for (int j = 0; j < size(tiedplayers)[0]; j++) {
          arma::uvec loc = find(Standings.col(0) == tiedplayers[j]);
          Standings(loc[0], 4) = H2H[j];
          Standings(loc[0], 5) = H2HW[j];
        }
      }
    }
    Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
    Standings.col(6) = getOPScores(Sets, Wins);
    Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
    Standings = Standings.rows(sort_index(Standings.col(6), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(5), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(4), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(2), "descend"));
    Standings = Standings.rows(sort_index(Standings.col(1), "descend"));
    Standings.col(3) = {1, 2, 3, 4, 5, 6};
  }
  Standings = Standings.rows(sort_index(Standings.col(0), "ascend"));
  return Standings.col(3);
}

// [[Rcpp::export]]
arma::mat getAllStandings(arma::mat PO) {
  arma::mat Standings(size(PO)[0], 6);
  // Rcout << vectorise(getStandings(vectorise(PO.row(0))), 1);
  for (int i = 0; i < size(Standings)[0]; i++) {
    Standings.row(i) = vectorise(getStandings(vectorise(PO.row(i))), 1);
  }
  return Standings;
}

// [[Rcpp::export]]
String formatTime(int time) {
  string str;
  if (time > 3600) {
    int h = time/3600;
    int m = (time % 3600)/60;
    int s = (time % 60);
    str = to_string(h) + "h " + to_string(m) + "m " + to_string(s) + "s                 ";
  } else {
    int m = (time % 3600)/60;
    int s = (time % 60);
    str = to_string(m) + "m " + to_string(s) + "s                 ";
  }
  return str;
}



// [[Rcpp::export]]
arma::mat getAllCombs(arma::mat W, int total, bool verbose = true) {
  arma::mat Standings(6, 6);
  // Progress p(total, true); //, pb);
  time_t t;
  int start = std::time(&t);
  int c = 0;
  for (double g1 = W(0,0); g1 <= W(0,1); g1++) {
    for (double g2 = W(1,0); g2 <= W(1,1); g2++) {
      for (double g3 = W(2,0); g3 <= W(2,1); g3++) {
        for (double g4 = W(3,0); g4 <= W(3,1); g4++) {
          for (double g5 = W(4,0); g5 <= W(4,1); g5++) {
            for (double g6 = W(5,0); g6 <= W(5,1); g6++) {
              for (double g7 = W(6,0); g7 <= W(6,1); g7++) {
                for (double g8 = W(7,0); g8 <= W(7,1); g8++) {
                  for (double g9 = W(8,0); g9 <= W(8,1); g9++) {
                    for (double g10 = W(9,0); g10 <= W(9,1); g10++) {
                      for (double g11 = W(10,0); g11 <= W(10,1); g11++) {
                        for (double g12 = W(11,0); g12 <= W(11,1); g12++) {
                          for (double g13 = W(12,0); g13 <= W(12,1); g13++) {
                            for (double g14 = W(13,0); g14 <= W(13,1); g14++) {
                              for (double g15 = W(14,0); g15 <= W(14,1); g15++) {
                                for (double g16 = W(15,0); g16 <= W(15,1); g16++) {
                                  for (double g17 = W(16,0); g17 <= W(16,1); g17++) {
                                    for (double g18 = W(17,0); g18 <= W(17,1); g18++) {
                                      for (double g19 = W(18,0); g19 <= W(18,1); g19++) {
                                        for (double g20 = W(19,0); g20 <= W(19,1); g20++) {
                                          for (double g21 = W(20,0); g21 <= W(20,1); g21++) {
                                            for (double g22 = W(21,0); g22 <= W(21,1); g22++) {
                                              for (double g23 = W(22,0); g23 <= W(22,1); g23++) {
                                                for (double g24 = W(23,0); g24 <= W(23,1); g24++) {
                                                  for (double g25 = W(24,0); g25 <= W(24,1); g25++) {
                                                    for (double g26 = W(25,0); g26 <= W(25,1); g26++) {
                                                      for (double g27 = W(26,0); g27 <= W(26,1); g27++) {
                                                        for (double g28 = W(27,0); g28 <= W(27,1); g28++) {
                                                          for (double g29 = W(28,0); g29 <= W(28,1); g29++) {
                                                            for (double g30 = W(29,0); g30 <= W(29,1); g30++) {
                                                              for (double g31 = W(30,0); g31 <= W(30,1); g31++) {
                                                                for (double g32 = W(31,0); g32 <= W(31,1); g32++) {
                                                                  for (double g33 = W(32,0); g33 <= W(32,1); g33++) {
                                                                    for (double g34 = W(33,0); g34 <= W(33,1); g34++) {
                                                                      for (double g35 = W(34,0); g35 <= W(34,1); g35++) {
                                                                        for (double g36 = W(35,0); g36 <= W(35,1); g36++) {
                                                                          for (double g37 = W(36,0); g37 <= W(36,1); g37++) {
                                                                            for (double g38 = W(37,0); g38 <= W(37,1); g38++) {
                                                                              for (double g39 = W(38,0); g39 <= W(38,1); g39++) {
                                                                                for (double g40 = W(39,0); g40 <= W(39,1); g40++) {
                                                                                  for (double g41 = W(40,0); g41 <= W(40,1); g41++) {
                                                                                    for (double g42 = W(41,0); g42 <= W(41,1); g42++) {
                                                                                      for (double g43 = W(42,0); g43 <= W(42,1); g43++) {
                                                                                        for (double g44 = W(43,0); g44 <= W(43,1); g44++) {
                                                                                          for (double g45 = W(44,0); g45 <= W(44,1); g45++) {
                                                                                            arma::vec current_standings = getStandings({g1, g2, g3, g4, g5, g6, g7, g8, g9, g10, g11,
                                                                                                                                       g12, g13, g14, g15, g16, g17, g18, g19, g20,
                                                                                                                                       g21, g22, g23, g24, g25, g26, g27, g28, g29,
                                                                                                                                       g30, g31, g32, g33, g34, g35, g36, g37, g38,
                                                                                                                                       g39, g40, g41, g42, g43, g44, g45});
                                                                                      
                                                                                            for (int i = 0; i < 6; i++) {
                                                                                              for (int j = 0; j < 6; j++) {
                                                                                                int cs = (int)current_standings[i];
                                                                                                if (cs-1 == j) {
                                                                                                  Standings(i,j)++; // = Standings(i,j) + 1;
                                                                                                }
                                                                                              }
                                                                                            }
                                                                                            //std::this_thread::sleep_for(std::chrono::nanoseconds(1000000));
                                                                                            c++;
                                                                                            // p.increment();
                                                                                            if (c % 200000 == 0 && verbose) {
                                                                                              warning(std::to_string(c));
                                                                                              //   std::stringstream strs;
                                                                                              //   double remaining = (total-c)*(std::time(&t)-start)/c;
                                                                                              //   string timestring = formatTime(remaining);
                                                                                              //   strs << c; //"Time Remaining: " << timestring;
                                                                                              //   std::string temp_str = strs.str();
                                                                                              //   char const* char_type = temp_str.c_str();
                                                                                              // 
                                                                                              //   REprintf("\r");
                                                                                              // REprintf("%s", char_type);
                                                                                              // REprintf("\r");
                                                                                            }
                                                                                            
                                                                                          }
                                                                                        }
                                                                                      }
                                                                                    }
                                                                                  }
                                                                                }
                                                                              }
                                                                            }
                                                                          }
                                                                        }
                                                                      }
                                                                    }
                                                                  }
                                                                }
                                                              }
                                                            }
                                                          }
                                                        }
                                                      }
                                                    }
                                                  }
                                                }
                                              }
                                            }
                                          }
                                        }
                                      }
                                    }
                                  }
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  // if (verbose) {
  //   std::stringstream strs;
  //   double remaining = std::time(&t)-start;
  //   string timestring = formatTime(remaining);
  //   strs << "Total Time: " << timestring;
  //   std::string temp_str = strs.str();
  //   char const* char_type = temp_str.c_str();
  //   Rcout << char_type;
  // }
  return Standings;
}

// [[Rcpp::export]]
arma::mat getAllCombs2(arma::mat W, int total, bool verbose = true) {
  arma::mat Standings(6, 6);
  // Progress p(total, true); //, pb);
  time_t t;
  int start = std::time(&t);
  int c = 0;
  for (double g1 = W(0,0); g1 <= W(0,1); g1++) {
    for (double g2 = W(1,0); g2 <= W(1,1); g2++) {
      for (double g3 = W(2,0); g3 <= W(2,1); g3++) {
        for (double g4 = W(3,0); g4 <= W(3,1); g4++) {
          for (double g5 = W(4,0); g5 <= W(4,1); g5++) {
            for (double g6 = W(5,0); g6 <= W(5,1); g6++) {
              for (double g7 = W(6,0); g7 <= W(6,1); g7++) {
                for (double g8 = W(7,0); g8 <= W(7,1); g8++) {
                  for (double g9 = W(8,0); g9 <= W(8,1); g9++) {
                    for (double g10 = W(9,0); g10 <= W(9,1); g10++) {
                      for (double g11 = W(10,0); g11 <= W(10,1); g11++) {
                        for (double g12 = W(11,0); g12 <= W(11,1); g12++) {
                          for (double g13 = W(12,0); g13 <= W(12,1); g13++) {
                            for (double g14 = W(13,0); g14 <= W(13,1); g14++) {
                              for (double g15 = W(14,0); g15 <= W(14,1); g15++) {
                                arma::vec current_standings = getStandings2({g1, g2, g3, g4, g5, g6, g7, g8,
                                                                            g9, g10, g11, g12, g13, g14, g15});
                                arma::vec PO = {g1, g2, g3, g4, g5, g6, g7, g8,
                                                g9, g10, g11, g12, g13, g14, g15};
                                int perms = 1;
                                for (int i = 0; i < 15; i++) {
                                  if ((PO[i] == 1 | PO[i] == 2) & (W(i,1)-W(i,0) == 3)) {
                                    perms = (W(i,1)-W(i,0))*perms;
                                  } else if (W(i,0) == 1 & PO[i] == 2 & (W(i,1)-W(i,0) == 2)) {
                                    perms = (W(i,1)-W(i,0))*perms;
                                  } else if (W(i,0) == 0 & PO[i] == 1 & (W(i,1)-W(i,0) == 2)) {
                                    perms = (W(i,1)-W(i,0))*perms;
                                  }
                                }
                                
                                for (int i = 0; i < 6; i++) {
                                  for (int j = 0; j < 6; j++) {
                                    int cs = (int)current_standings[i];
                                    if (cs-1 == j) {
                                      Standings(i,j) = Standings(i,j) + perms;
                                    }
                                  }
                                }
                                //std::this_thread::sleep_for(std::chrono::nanoseconds(1000000));
                                c++;
                                // p.increment();
                                if (c % 200000 == 0 && verbose) {
                                  warning(std::to_string(c));
                                  //   std::stringstream strs;
                                  //   double remaining = (total-c)*(std::time(&t)-start)/c;
                                  //   string timestring = formatTime(remaining);
                                  //   strs << c; //"Time Remaining: " << timestring;
                                  //   std::string temp_str = strs.str();
                                  //   char const* char_type = temp_str.c_str();
                                  // 
                                  //   REprintf("\r");
                                  // REprintf("%s", char_type);
                                  // REprintf("\r");
                                }
                              }
                            }
                          }
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  // if (verbose) {
  //   std::stringstream strs;
  //   double remaining = std::time(&t)-start;
  //   string timestring = formatTime(remaining);
  //   strs << "Total Time: " << timestring;
  //   std::string temp_str = strs.str();
  //   char const* char_type = temp_str.c_str();
  //   Rcout << char_type;
  // }
  return Standings;
}


