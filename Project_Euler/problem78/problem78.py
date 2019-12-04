from itertools import *
from math import pow

part_dict = {}
check = 1000000

def part(num):
	if num < 0:
		return 0
	if num == 0:
		return 1
	if num in part_dict:
		return part_dict[num]
	else:
		lst1 = takewhile(lambda x: x*(3*x-1)/2 <= num, count(1))
		lst2 = takewhile(lambda x: x*(3*x-1)/2 <= num, count(-1, -1))
		sol = 0
		for k in lst1:
			sol += (pow(-1, k-1) * part(num - k*(3*k-1)/2))
			sol = sol % check
		for k in lst2:
			sol += (pow(-1, k-1) * part(num - k*(3*k-1)/2))
			sol = sol % check
		part_dict[num] = sol % check
		print("p(%d) = %d" % (num, sol))
		return sol

def main():
	lst = dropwhile(lambda x: part(x) % check != 0, count(1))
	head = islice(lst, 1)
	for k in head:
		print(k)

if __name__=="__main__":
	main()