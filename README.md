# Goal

   The goal of my project was an interpreter and typechecker for a
   language that implemented pairs and tagged unions

# Starting Point

   My starting point was interpWithEnv from hw06 which used L5

# How close I got

   I was able to complete the interpreter as far as I can tell, as it passes all of my cases.
   I made a decent amount of progress on the type checker, but there were a few key expressions
   I failed to complete, including the FunE and the AppE. I believe I completed the Pairs checking
   successfully as those tests pass as well, however my tagged unions tests result in Nothing. I'm
   Not sure exactly what I'm missing but I believe it has something to do with my LeftE and RightE
   taking Just ts, I think my CaseE may be correct.

# What was harder and easier

   The interpreter went fairly smoothly, I think it was the most intuitive. The type checker was more
   difficult, although I found some cases easier than others. It certianly took a second to make the
   transition to the Mega language although that just took a little time.

# To run

   From the parent directory run <make fp>

   src/FP.hs contains, in order:

   Translations
   Interpreter
   Type Checker
   Tests
   
