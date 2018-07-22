# Hybrid-LNS

This repository contains the implementation written in Clojure of the method **Hybrid Large Neighborhood Search** proposed by [Akpinar (2016)](https://dx.doi.org/10.1016/j.eswa.2016.05.023) to solve Capacitated Vehicle Routing (CVRP) instances. 
It is the product of the [Term Paper (in Portuguese)](https://gist.github.com/lucascb/f9fef4e3e70606592b236361dedeba24) of my Computer Science undergraduation in Federal University of Uberlândia.
The algorithm accepts instance files in JSON format and produces a JSON result file with the solution found. You can check examples of CVRP instances on _resources_ directory in this repository or check [CVRPlib-Parser](https://github.com/lucascb/cvrplib_parser) repository.

## Requirements

Java Runtime Environment 1.8+ is needed to run the JAR file. You may need Clojure 1.8+ and [Leiningen](https://leininge.org) if you want to work in this code.

## Usage

You can run this algorithm using command line, passing the parameters file and the instance to be solved:

    $ java -jar hybrid_lns.jar hybrid_lns.params resources/A-n32-k5.json

Passing a directory will run the algorithm on all instances in it:

    $ java -jar hybrid_lns.jar hybrid_lns.params resources/

The results can be found in the out/ directory.

## License

This project offers no warranty. You can clone it, download it or make your own version of this algorithm.

