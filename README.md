This repository holds the code, used to run the experiments, for the paper:

[Evaluation of the impact on energy consumption of lazy versus strict evaluation of Haskell data-structures](http://green-haskell.github.io/energy_consumption-lazy_vs_strict_evaluation_of_data-structures/index.html)

Execute the following commands in the desired directory.

\# To retrieve the code (this repository):

git clone https://github.com/green-haskell/maps-LvsS.git


\# To install the dependencies, required to run the benchmarks:

bash ./maps-LvsS/installDependencies.sh


\#To run all benchmarks, as root:

cd maps-LvsS ; make 2>&1 | tee fullOutput.txt


