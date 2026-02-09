# Part 1
# A: Probability of even
# B: Probability of roll being greater than 3
# A = {2, 4, 6}
# B = {4, 5, 6}
# P(A) = 1/2
# P(B) = 1/2
# A \cap B = {4, 6}
# P(A \cap B) = 2/6 = 1/3

print("_____ Part 1 _____")

dice_conditional_probability <- function(B, A_and_B) {
    A_given_B <- A_and_B / B
    return(A_given_B)
}

p_a_given_b <- dice_conditional_probability(1/2, 1/3)
print(paste("p(A|B) =", p_a_given_b))


# Part 2
print("_____ Part 2 _____")
# Seeding for debugging purposes
set.seed(1)
dice_simulation <- function(n){
    rolls <- sample(1:6, n, replace = TRUE)
    # A
    even_rolls <- length(rolls[rolls %% 2 == 0]) 
    # B
    greater_than_three_rolls <- length(rolls[rolls > 3])
    # A \cap B
    even_and_greater_than_three_rolls <- length(rolls[rolls %% 2 == 0 & rolls > 3])
    # A | B
    a = even_rolls / n
    b = greater_than_three_rolls / n

    a_given_b <- dice_conditional_probability(greater_than_three_rolls, even_and_greater_than_three_rolls)
    return(list("P(A)"=a, "P(B)"=b, "P(A|B)"=a_given_b))
}

# 10,000 rolls simulation
results_10k = dice_simulation(10000)
print("Expected P(A) = 0.5")
print(paste("P(A) = ", results_10k$`P(A)`))
print("Expected P(B) = 0.5")
print(paste("P(B) = ", results_10k$`P(B)`))
print("Expected P(A|B) = 0.6667")
print(paste("P(A|B) = ", results_10k$`P(A|B)`))



# Part 3 Non Traditional Dice Tossing With Weighted Dice
# 1: 0.1
# 2: 0.1
# 3: 0.2
# 4: 0.2
# 5: 0.2
# 6: 0.2


# C: Probability of number rolled being multiple of 3 {3, 6}
# D: Number rolled is greater than or equal to 4 {4, 5, 6}

# C \cap D = {6}

# P(C) = P(3) + P(6) = 0.2 + 0.2 = 0.4
# P(C \cap D) = P(6) = 0.2
# P(D) = P(4) + P(5) + P(6) = 0.2 + 0.2 + 0.2 = 0.6
# P(C | D) = P(C \cap D) / P(D) = 0.2 / 0.6 = 0.3333

# For Debugging Purposes
set.seed(1)
print("_____ Part 3 _____")
probabilities = c(0.1, 0.1, 0.2, 0.2, 0.2, 0.2)
weighted_dice_simulation <- function(n, probabilities) {
    # Generate n random numbers between 0 and 1
    rolls = sample(1:6, n, replace=TRUE, prob=probabilities)

    # Assign outcomes based on bins
    n = length(rolls)
    p_c = length(rolls[rolls %% 3 == 0])/n
    p_d = length(rolls[rolls >= 4])/n

    p_c_and_d = length(rolls[rolls %% 3 == 0 & rolls >= 4])/n

    p_c_given_d = dice_conditional_probability(p_d, p_c_and_d)
    return(list("P(C)"=p_c, "P(D)"=p_d, "P(C and D)"=p_c_and_d, "P(C|D)"=p_c_given_d))
}

results_weighted = weighted_dice_simulation(10000, probabilities)
print("Expected P(C) = 0.4")
print(paste("P(C) = ", results_weighted$`P(C)`))

print("Expected P(D) = 0.6")
print(paste("P(D) = ", results_weighted$`P(D)`))

print("Expected P(C and D) = 0.2")
print(paste("P(C and D) = ", results_weighted$`P(C and D)`))

print("Expected P(C|D) = 0.3333")
print(paste("P(C|D) = ", results_weighted$`P(C|D)`))