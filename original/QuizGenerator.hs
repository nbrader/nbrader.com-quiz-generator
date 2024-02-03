#!/usr/bin/env stack
-- stack --resolver lts-21.22 ghci --package random-1.2.1.1 --package time-1.12.2

-- asks for one of paired values
-- gives hints at accepted forms of answer
-- includes questions from previous quizzes
--  - greek alphabet
--  - state capitals
--  - morse code
-- make it so you can select the categories to include in a given quiz
-- records every answered question with following data to a new file for every program run (with time and date of programs start in file name) (file is written as program runs so that even crashed programs give data)
--  - time-date when question was asked
--  - time-date when question was answered
--  - time since program start when question was asked
--  - time since program start when question was answered
--  - what the question was
--  - what the answer was
-- if question is answered correctly, say so to use via commandline printout
-- if question is answered incorrectly, say so and give correct answer
-- give running score of correct vs total in form "[5 Answered, 2 Correct, 1 in Current Streak]"
-- make separate part of the program which can parse log files (allows you to pick the log file to summarize or instead summarize all of them or those withing a date range) and present a summary of the data

-- import Data.List (intercalate, groupBy, sortOn)
-- import Data.Maybe (fromJust, isJust)
-- import Data.Function (on)
import Data.Time
import System.Environment
import System.Random
-- import System.Directory

-- import Split
-- import Data.Maybe
-- import Text.Printf

pick :: RandomGen g => g -> [a] -> [a]
pick g xs = fmap (xs !!) $ randomRs (0, length xs - 1) g

pad :: Char -> [String] -> [String]
pad c xs = ys where
  (ys, n)         = foldr cons ([],0) xs
  cons x (acc, m) = ((replicate (n - m') c ++ x) : acc, max m m')
    where m' = length x

main = do
  printIntro
  askWhichProgramToRun

printIntro = do
  putStrLn "Welcome to Nathan's Quiz Generator!"
  utcTime <- getCurrentTime
  timeZone <- getCurrentTimeZone
  let localTime = utcToLocalTime timeZone utcTime
  mapM_ putStrLn $ pad '_' ["The local time:", show localTime, "...but the logs are stored using UTCTime:", show utcTime]

askWhichProgramToRun = do
  putStrLn "1 - Start Full Quiz"
  putStrLn "2 - Start Custom Quiz"
  putStrLn "3 - Create Results Summary"
  userReplyStr <- getLine
  case userReplyStr of
    "1" -> startFullQuiz
    "2" -> startCustomQuiz
    "3" -> createResultsSummary

startFullQuiz = do
  putStrLn "startFullQuiz"
  -- g <- mkStdGen 0
  g <- getStdGen
  let qs = pick g (
           greekLetterASCIIArtLowercaseToName
        ++ greekLetterASCIIArtUppercaseToName
        ++ brailleASCIIArtToLetter
        ++ morseCodeToLetter
        ++ map reverseQuestion morseCodeToLetter
        ++ usaStateToCapital
        ++ map reverseQuestion usaStateToCapital
        )
  mapM_ pose $ take 20 qs

pose :: Question -> IO ()
pose q = do
  putStr $ show (category q) ++ ":\n" ++ prompt q
  putStr $ "\n"
  userReplyStr <- getLine
  if userReplyStr == trueAnswer q
   then do
     putStr "Correct!\n\n"
   else do
     putStr "False!\n"
     putStr $ "It was " ++ show (trueAnswer q) ++ "\n\n"

-- allow category to picked
-- allow randomisation to be done with/without replacement
startCustomQuiz = do
  putStrLn "startCustomQuiz not implemented yet"

-- 
createResultsSummary = do
  putStrLn "createResultsSummary not implemented yet"

data Category = Reverse Category
              | GreekLetterLowercaseToName
              | GreekLetterUppercaseToName
              | GreekLetterASCIIArtLowercaseToName
              | GreekLetterASCIIArtUppercaseToName
              | BrailleASCIIArtToLetter
              | MorseCodeToLetter
              | USAStateToCapital
              | MiscToLongAnswer deriving Show

data Question = Question {category :: Category, prompt :: String, trueAnswer :: String} deriving Show

reverseQuestion q = q{category = Reverse (category q), prompt = trueAnswer q, trueAnswer = prompt q}

greekLetterLowercaseToName :: [Question]
greekLetterLowercaseToName = map (\(p, a) -> Question GreekLetterUppercaseToName p a) [
        ("α","alpha"),
        ("β","beta"),
        ("γ","gamma"),
        ("δ","delta"),
        ("ε","epsilon"),
        ("ζ","zeta"),
        ("η","eta"),
        ("θ","theta"),
        ("ι","iota"),
        ("κ","kappa"),
        ("λ","lambda"),
        ("μ","mu"),
        ("ν","nu"),
        ("ξ","xi"),
        ("ο","omicron"),
        ("π","pi"),
        ("ρ","rho"),
        ("σ","sigma"),
        ("τ","tau"),
        ("υ","upsilon"),
        ("φ","phi"),
        ("χ","chi"),
        ("ψ","psi"),
        ("ω","omega")
    ]

greekLetterUppercaseToName :: [Question]
greekLetterUppercaseToName = map (\(p, a) -> Question GreekLetterUppercaseToName p a)[
        ("Α","alpha"),
        ("Β","beta"),
        ("Γ","gamma"),
        ("Δ","delta"),
        ("Ε","epsilon"),
        ("Ζ","zeta"),
        ("Η","eta"),
        ("Θ","theta"),
        ("Ι","iota"),
        ("Κ","kappa"),
        ("Λ","lambda"),
        ("Μ","mu"),
        ("Ν","nu"),
        ("Ξ","xi"),
        ("Ο","omicron"),
        ("Π","pi"),
        ("Ρ","rho"),
        ("Σ","sigma"),
        ("Τ","tau"),
        ("Υ","upsilon"),
        ("Φ","phi"),
        ("Χ","chi"),
        ("Ψ","psi"),
        ("Ω","omega")
    ]

greekLetterASCIIArtLowercaseToName :: [Question]
greekLetterASCIIArtLowercaseToName = map (\(p, a) -> Question GreekLetterASCIIArtLowercaseToName p a) [
        (alphaLowerRepr,"alpha"),
        (betaLowerRepr,"beta"),
        (gammaLowerRepr,"gamma"),
        (deltaLowerRepr,"delta"),
        (epsilonLowerRepr,"epsilon"),
        (zetaLowerRepr,"zeta"),
        (etaLowerRepr,"eta"),
        (thetaLowerRepr,"theta"),
        (iotaLowerRepr,"iota"),
        (kappaLowerRepr,"kappa"),
        (lambdaLowerRepr,"lambda"),
        (muLowerRepr,"mu"),
        (nuLowerRepr,"nu"),
        (xiLowerRepr,"xi"),
        (omicronLowerRepr,"omicron"),
        (piLowerRepr,"pi"),
        (rhoLowerRepr,"rho"),
        (sigmaLowerRepr,"sigma"),
        (tauLowerRepr,"tau"),
        (upsilonLowerRepr,"upsilon"),
        (phiLowerRepr,"phi"),
        (chiLowerRepr,"chi"),
        (psiLowerRepr,"psi"),
        (omegaLowerRepr,"omega")
    ]

greekLetterASCIIArtUppercaseToName :: [Question]
greekLetterASCIIArtUppercaseToName = map (\(p, a) -> Question GreekLetterUppercaseToName p a)[
        (alphaUpperRepr,"alpha"),
        (betaUpperRepr,"beta"),
        (gammaUpperRepr,"gamma"),
        (deltaUpperRepr,"delta"),
        (epsilonUpperRepr,"epsilon"),
        (zetaUpperRepr,"zeta"),
        (etaUpperRepr,"eta"),
        (thetaUpperRepr,"theta"),
        (iotaUpperRepr,"iota"),
        (kappaUpperRepr,"kappa"),
        (lambdaUpperRepr,"lambda"),
        (muUpperRepr,"mu"),
        (nuUpperRepr,"nu"),
        (xiUpperRepr,"xi"),
        (omicronUpperRepr,"omicron"),
        (piUpperRepr,"pi"),
        (rhoUpperRepr,"rho"),
        (sigmaUpperRepr,"sigma"),
        (tauUpperRepr,"tau"),
        (upsilonUpperRepr,"upsilon"),
        (phiUpperRepr,"phi"),
        (chiUpperRepr,"chi"),
        (psiUpperRepr,"psi"),
        (omegaUpperRepr,"omega")
    ]

brailleASCIIArtToLetter :: [Question]
brailleASCIIArtToLetter = map (\(p, a) -> Question BrailleASCIIArtToLetter p a) [
        (".  \n   \n   ","a"),
        (".  \n.  \n   ","b"),
        (". .\n   \n   ","c"),
        (". .\n  .\n   ","d"),
        (".  \n  .\n   ","e"),
        (". .\n.  \n   ","f"),
        (". .\n. .\n   ","g"),
        (".  \n. .\n   ","h"),
        ("  .\n.  \n   ","i"),
        ("  .\n. .\n   ","j"),
        (".  \n   \n.  ","k"),
        (".  \n.  \n.  ","l"),
        (". .\n   \n.  ","m"),
        (". .\n  .\n.  ","n"),
        (".  \n  .\n.  ","o"),
        (". .\n.  \n.  ","p"),
        (". .\n. .\n.  ","q"),
        (".  \n. .\n.  ","r"),
        ("  .\n.  \n.  ","s"),
        ("  .\n. .\n.  ","t"),
        (".  \n   \n. .","u"),
        (".  \n.  \n. .","v"),
        ("  .\n. .\n  .","w"),
        (". .\n   \n. .","x"),
        (". .\n  .\n. .","y"),
        (".  \n  .\n. .","z"),
        ("  .   .  \n  .      \n. .      ","1"),
        ("  .   .  \n  .   .  \n. .      ","2"),
        ("  .   . .\n  .      \n. .      ","3"),
        ("  .   . .\n  .     .\n. .      ","4"),
        ("  .   .  \n  .     .\n. .      ","5"),
        ("  .   . .\n  .   .  \n. .      ","6"),
        ("  .   . .\n  .   . .\n. .      ","7"),
        ("  .   .  \n  .   . .\n. .      ","8"),
        ("  .     .\n  .   .  \n. .      ","9"),
        ("  .     .\n  .   . .\n. .      ","0")
    ]

morseCodeToLetter = map (\(p, a) -> Question MorseCodeToLetter p a) [
        (".-","a"),
        ("-...","b"),
        ("-.-.","c"),
        ("-..","d"),
        (".","e"),
        ("..-.","f"),
        ("--.","g"),
        ("....","h"),
        ("..","i"),
        (".---","j"),
        ("-.-","k"),
        (".-..","l"),
        ("--","m"),
        ("-.","n"),
        ("---","o"),
        (".--.","p"),
        ("--.-","q"),
        (".-.","r"),
        ("...","s"),
        ("-","t"),
        ("..-","u"),
        ("...-","v"),
        (".--","w"),
        ("-..-","x"),
        ("-.--","y"),
        ("--..","z")
    ]

usaStateToCapital = map (\(p, a) -> Question USAStateToCapital p a) [
        ("Alabama","Montgomery"),
        ("Alaska","Juneau"),
        ("Arizona","Phoenix"),
        ("Arkansas","Little Rock"),
        ("California","Sacramento"),
        ("Colorado","Denver"),
        ("Connecticut","Hartford"),
        ("Delaware","Dover"),
        ("Florida","Tallahassee"),
        ("Georgia","Atlanta"),
        ("Hawaii","Honolulu"),
        ("Idaho","Boise"),
        ("Illinois","Springfield"),
        ("Indiana","Indianapolis"),
        ("Iowa","Des Moines"),
        ("Kansas","Topeka"),
        ("Kentucky","Frankfort"),
        ("Louisiana","Baton Rouge"),
        ("Maine","Augusta"),
        ("Maryland","Annapolis"),
        ("Massachusetts","Boston"),
        ("Michigan","Lansing"),
        ("Minnesota","Saint Paul"),
        ("Mississippi","Jackson"),
        ("Missouri","Jefferson City"),
        ("Montana","Helena"),
        ("Nebraska","Lincoln"),
        ("Nevada","Carson City"),
        ("New Hampshire","Concord"),
        ("New Jersey","Trenton"),
        ("New Mexico","Santa Fe"),
        ("New York","Albany"),
        ("North Carolina","Raleigh"),
        ("North Dakota","Bismarck"),
        ("Ohio","Columbus"),
        ("Oklahoma","Oklahoma City"),
        ("Oregon","Salem"),
        ("Pennsylvania","Harrisburg"),
        ("Rhode Island","Providence"),
        ("South Carolina","Columbia"),
        ("South Dakota","Pierre"),
        ("Tennessee","Nashville"),
        ("Texas","Austin"),
        ("Utah","Salt Lake City"),
        ("Vermont","Montpelier"),
        ("Virginia","Richmond"),
        ("Washington","Olympia"),
        ("West Virginia","Charleston"),
        ("Wisconsin","Madison"),
        ("Wyoming","Cheyenne")
    ]

miscToLongAnswer = map (\(p, a) -> Question MiscToLongAnswer p a) [
        ("What makes a mortice lock a mortice lock?","A mortise lock (also spelled mortice lock in British English) is a lock that requires a pocket—the mortise—to be cut into the edge of the door or piece of furniture into which the lock is to be fitted.")
    ]

alphaUpperRepr = "       \n  ___  \n / _ \\ \n| |_| |\n|  _  |\n| | | |\n|_| |_|\n       \n       "
betaUpperRepr = "       \n ____  \n|  _ \\ \n| |_) )\n|  _ ( \n| |_) )\n|____/ \n       \n       "
gammaUpperRepr = "       \n _____ \n|  ___)\n| |    \n| |    \n| |    \n|_|    \n       \n       "
deltaUpperRepr = "          \n          \n    /\\    \n   /  \\   \n  / /\\ \\  \n / /__\\ \\ \n/________\\\n          \n          "
epsilonUpperRepr = "       \n _____ \n|  ___)\n| |_   \n|  _)  \n| |___ \n|_____)\n       \n       "
zetaUpperRepr = "       \n ______\n(___  /\n   / / \n  / /  \n / /__ \n/_____)\n       \n       "
etaUpperRepr = "       \n _   _ \n| | | |\n| |_| |\n|  _  |\n| | | |\n|_| |_|\n       \n       "
thetaUpperRepr = "        \n  ____  \n / __ \\ \n| |__| |\n|  __  |\n| |__| |\n \\____/ \n        \n        "
iotaUpperRepr = "     \n ___ \n(   )\n | | \n | | \n | | \n(___)\n     \n     "
kappaUpperRepr = "       \n _   __\n| | / /\n| |/ / \n|   <  \n| |\\ \\ \n|_| \\_\\\n       \n       "
thetaUpperRepr' = "       \n  ____ \n / __ \\\n| |__| \n|  __  \n| |__| \n \\____/\n       \n       "
iotaUpperRepr' = "      \n  ___  \n (   )|\n| | | |\n| | | |\n| | | |\n (___)|\n       \n       "
kappaUpperRepr' = "       \n_   __\n | / /\n |/ / \n   <  \n |\\ \\ \n_| \\_\\\n      \n      "
lambdaUpperRepr = "          \n          \n    /\\    \n   /  \\   \n  / /\\ \\  \n / /  \\ \\ \n/_/    \\_\\\n          \n          "
muUpperRepr = "         \n __   __ \n|  \\ /  |\n|   v   |\n| |\\_/| |\n| |   | |\n|_|   |_|\n         \n         "
nuUpperRepr = "       \n _   _ \n| \\ | |\n|  \\| |\n|     |\n| |\\  |\n|_| \\_|\n       \n       "
xiUpperRepr = "       \n _____ \n(_____)\n  ___  \n (___) \n _____ \n(_____)\n       \n       "
omicronUpperRepr = "       \n  ___  \n / _ \\ \n| | | |\n| | | |\n| |_| |\n \\___/ \n       \n       "
piUpperRepr = "         \n _______ \n(   _   )\n | | | | \n | | | | \n | | | | \n |_| |_| \n         \n         "
rhoUpperRepr = "       \n ____  \n|  _ \\ \n| |_) )\n|  __/ \n| |    \n|_|    \n       \n       "
sigmaUpperRepr = "       \n______ \n\\  ___)\n \\ \\   \n  > >  \n / /__ \n/_____)\n       \n       "
tauUpperRepr = "       \n _____ \n(_   _)\n  | |  \n  | |  \n  | |  \n  |_|  \n       \n       "
upsilonUpperRepr = "         \n __   __ \n(_ \\ / _)\n  \\ v /  \n   | |   \n   | |   \n   |_|   \n         \n         "
phiUpperRepr = "         \n    _    \n  _| |_  \n /     \\ \n( (| |) )\n \\_   _/ \n   |_|   \n         \n         "
chiUpperRepr = "       \n__   __\n\\ \\ / /\n \\ v / \n  > <  \n / ^ \\ \n/_/ \\_\\\n       \n       "
psiUpperRepr = "         \n _  _  _ \n| || || |\n| \\| |/ |\n \\_   _/ \n   | |   \n   |_|   \n         \n         "
omegaUpperRepr = "          \n   ____   \n  / __ \\  \n | |  | | \n | |  | | \n _\\ \\/ /_ \n(___||___)\n          \n          "

alphaLowerRepr = "        \n        \n        \n  __  __\n /  \\/ /\n( ()  < \n \\__/\\_\\\n        \n        "
betaLowerRepr = "       \n  ___  \n / _ \\ \n| |_) )\n|  _ < \n| |_) )\n|  __/ \n| |    \n|_|    "
gammaLowerRepr = "       \n       \n       \n _   _ \n( \\ / )\n \\ v / \n  | |  \n  | |  \n  |_|  "
deltaLowerRepr = "       \n   __  \n  / _) \n  \\ \\  \n / _ \\ \n( (_) )\n \\___/ \n       \n       "
epsilonLowerRepr = "     \n     \n     \n ___ \n/ __)\n> _) \n\\___)\n     \n     "
zetaLowerRepr = "       \n_____  \n\\__  ) \n  / /  \n / /   \n| |__  \n \\__ \\ \n    ) )\n   (_/ "
etaLowerRepr = "       \n       \n       \n _ __  \n| '_ \\ \n| | | |\n|_| | |\n    | |\n    |_|"
thetaLowerRepr = "       \n  ___  \n / _ \\ \n| |_| |\n|  _  |\n| |_| |\n \\___/ \n       \n       "
iotaLowerRepr = "    \n    \n    \n _  \n| | \n| | \n \\_)\n    \n    "
kappaLowerRepr = "      \n      \n      \n _  __\n| |/ /\n|   < \n|_|\\_\\\n      \n      "
lambdaLowerRepr = "       \n__     \n\\ \\    \n \\ \\   \n  > \\  \n / ^ \\ \n/_/ \\_\\\n       \n       "
muLowerRepr = "       \n       \n       \n _   _ \n| | | |\n| |_| |\n| ._,_|\n| |    \n|_|    "
nuLowerRepr = "      \n      \n      \n _  __\n| |/ /\n| / / \n|__/  \n      \n      "
xiLowerRepr = "__     \n\\ \\__  \n > __) \n( (_   \n > _)  \n( (__  \n \\__ \\ \n    ) )\n   (_/ "
omicronLowerRepr = "       \n       \n       \n  ___  \n / _ \\ \n( (_) )\n \\___/ \n       \n       "
piLowerRepr = "        \n        \n        \n ______ \n(  __  )\n | || | \n |_||_| \n        \n        "
rhoLowerRepr = "       \n       \n       \n  ___  \n / _ \\ \n| |_) )\n|  __/ \n| |    \n|_|    "
sigmaLowerRepr = "       \n       \n       \n  ____ \n /  ._)\n( () ) \n \\__/  \n       \n       "
tauLowerRepr = "     \n     \n     \n ___ \n(   )\n | | \n  \\_)\n     \n     "
upsilonLowerRepr = "       \n       \n       \n _   _ \n| | | |\n| |_| |\n \\___/ \n       \n       "
phiLowerRepr = "         \n    _    \n   | |   \n  _| |_  \n /     \\ \n( (| |) )\n \\_   _/ \n   | |   \n   |_|   "
chiLowerRepr = "       \n       \n       \n__   __\n\\ \\ / /\n \\ v / \n  > <  \n / ^ \\ \n/_/ \\_\\"
psiLowerRepr = "         \n         \n         \n _  _  _ \n| || || |\n| \\| |/ |\n \\_   _/ \n   | |   \n   |_|   "
omegaLowerRepr = "           \n           \n           \n  __   __  \n / / _ \\ \\ \n| |_/ \\_| |\n \\___^___/ \n           \n           "

data Quiz = Quiz {categories :: [Question]} deriving Show

data UserReply = UserReply {
    usrRpPrompt :: String,
    usrRpTrueAnswer :: String,
    usrRpSubmission :: String,
    usrRpPromptDateTime :: UTCTime,
    usrRpSubmissionDateTime :: UTCTime,
    usrRpPromptMillisecondsSinceStart :: Int,
    usrRpSubmissionMillisecondsSinceStart :: Int} deriving Show