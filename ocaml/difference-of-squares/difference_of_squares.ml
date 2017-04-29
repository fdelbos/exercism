let sqr nb = nb * nb

let square_of_sum nb =
  sqr (nb * (nb + 1) / 2)

let sum_of_squares nb =
  nb * (nb + 1) * ((2 * nb) + 1) / 6

let difference_of_squares nb =
  (square_of_sum nb) - (sum_of_squares nb)
