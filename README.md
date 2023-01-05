# stm-haskell

Concurrent algorithms in Haskell using software transactional memory with the STM monad. The programs here implement the ideas in the 2005 paper [Composable Memory Transactions](https://www.microsoft.com/en-us/research/wp-content/uploads/2005/01/2005-ppopp-composable.pdf). The two main gists are 1) no explicit locks or mutexes are used in the code, and 2) memory transactions are *composable*, freely and robustly so with no added risk of finicky lock management bugs.

The programs below are a mixture of classic concurrency problems from computer science, and problems from [The Little Book of Semaphores](https://greenteapress.com/wp/semaphores/). They are ordered roughly by complexity, starting with a simple example of 3 agents contending for shared resources.

Problem | Description / Notes
--- | ---
[Cigarette Smokers](src/CigaretteSmokers.hs) | Three smokers at a table contend for materials set down by an agent
[Sleeping Barber](src/SleepingBarber.hs) | Customers contend for a single barber at a barbershop, with 3 chairs in the waiting area
[Multiple Barbers](src/MultipleBarbers.hs) | Like *Sleeping Barber* but there are now 3 barbers, with a couch for 4 waiting customers and standing room for 8 more
[Sushi Bar 1](src/SushiBar1.hs) | Customers queue up to visit a 5-seat sushi bar, but sometimes have to wait for all customers to leave before sitting down
[Sushi Bar 2](src/SushiBar2.hs) | Like *Sushi Bar 1* but a more correct interpretation of the problem
[Santa Claus](src/SantaClaus.hs) | 9 reindeer and 10 elves contend for Santa's attention

