#Zadanie 2

install.packages("nortest")

# Test zgodności Kolmogorowa-Lillieforsa dla grupy 1
test1 <- lillie.test(grupa1)

# Test zgodności Kolmogorowa-Lillieforsa dla grupy 2
test2 <- lillie.test(grupa2)

# Wyniki testów
cat("Wartość p-value dla grupy 1:", test1$p.value, "\n")
cat("Wartość p-value dla grupy 2:", test2$p.value, "\n")

# Interpretacja wyników
if (test1$p.value > 0.05) {
  cat("Na podstawie testu zgodności Kolmogorowa-Lillieforsa można stwierdzić, że wyniki sprawdzianów w grupie 1 mają rozkład normalny.\n")
} else {
  cat("Na podstawie testu zgodności Kolmogorowa-Lillieforsa można stwierdzić, że wyniki sprawdzianów w grupie 1 nie mają rozkładu normalnego. 1.\n")
}

if (test2$p.value > 0.05) {
  cat("Na podstawie testu zgodności Kolmogorowa-Lillieforsa można stwierdzić, że wyniki sprawdzianów w grupie 2 mają rozkład normalny.\n")
} else {
  cat("Na podstawie testu zgodności Kolmogorowa-Lillieforsa można stwierdzić, że wyniki sprawdzianów w grupie 1 nie mają rozkładu normalnego.\n")
}
