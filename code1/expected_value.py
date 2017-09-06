import random

def f(x, c=1.0/30.0):
    return c*(x+1)*(x+1)

# choices = [(value, weight), ...]
def weighted_choice(choices):
   total = sum(w for c, w in choices)
   r = random.uniform(0, total)
   upto = 0
   for c, w in choices:
      if upto + w >= r:
         return c
      upto += w
   assert False, "Shouldn't get here"

def simulate_expected_value(function, x_values, iterations=1000):
    choices = list(map(lambda x: (x, f(x)), x_values))

    total = 0
    for n in range(iterations):
        sample = weighted_choice(choices)
        total += sample

    return float(total) / float(iterations)

def f(x, c=10.0):
    return float(x) / float(c)

def g(x, c=1.0/30.0):
    return float(c)*(x+1)**2.0

e1 = simulate_expected_value(f, (1,2,3,4))
print("E(X) for problem 2a: %s" % e1)

e2 = simulate_expected_value(g, (0,1,2,3))
print("E(X) for problem 2b: %s" % e2)
