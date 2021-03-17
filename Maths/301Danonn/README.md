# B-MAT-500-PAR-5-1-301danonn-tony.zhou

You’ve been hired by Dannon to sort all the information from their databases. There are a few thousand
petabytes of data, and it is critical to implement the most optimal sort possible.

Therefore, you are going to benchmark different sorting algorithms by comparing their execution speed, or
rather the number of elementary operations performed by the algorithm (in order to be independent of
the machine and its power). This is known as the time complexity of an algorithm.

There are numerous elementary operations that can be relevant: variable declaration, variable assignment,
variable access, function call, calculation, comparison, test, etc. To keep it simple, you will only count the
number of comparisons between the given elements.

You have to implement and benchmark the following algorithms:
• Selection sort
• Insertion sort
• Bubble sort
• Quicksort
• Merge sort

To ensure you get the proper results, please follow these implementation guidelines:
    • Whenever you go through the list of elements (whether you are looking for, selecting or inserting an
      element), always go from left to right.
    • For quicksort, always pick the first element as pivot, and keep the relative order of the elements in
      both partitions.
    • Don’t optimize the algorithms, or you will skew the results. For example, don’t use a flag to stop bubble
      sort early when nothing was swapped.


USAGE
∇ Terminal - + x ∼/B-MAT-500> ./301dannon -h
USAGE
./301dannon file

DESCRIPTION
    file file that contains the numbers to be sorted, separated by spaces

Sort functions are obviously unauthorized, regardless of the language. You must code
the sorts yourself.

SUGGESTED BONUSES
• Add other algorithms
• Benchmark some optimizations of the algorithms to see which ones are truly efficient
• Produce statistics, plot graphs of the number of operations according to the size of the data

EXAMPLES
∇ Terminal - + x ∼/B-MAT-500> cat list
3.3 5 9.89 -6
∼/B-MAT-500> ./301dannon list
4 elements
Selection sort: 6 comparisons
Insertion sort: 4 comparisons
Bubble sort: 6 comparisons
Quicksort: 4 comparisons
Merge sort: 5 comparisons

∇ Terminal - + x ∼/B-MAT-500> cat list
-564 1340 42 129 858 1491 1508 246 -1281 655 1506 306 290 -768 116 765 -48 -512 2598 42 2339
∼/B-MAT-500> ./301dannon list
21 elements
Selection sort: 210 comparisons
Insertion sort: 125 comparisons
Bubble sort: 210 comparisons
Quicksort: 80 comparisons
Merge sort: 67 comparisons