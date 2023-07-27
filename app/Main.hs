import Data.List
import System.Random

uppercaseLetters :: String
uppercaseLetters = ['A' .. 'Z']

lowercaseLetters :: String
lowercaseLetters = ['a' .. 'z']

digits :: String
digits = ['0' .. '9']

specialChars :: String
specialChars = "!@#$%^&*()_+-=[]{}|;:,.<>?/`~"

allChars :: String
allChars = uppercaseLetters ++ lowercaseLetters ++ digits ++ specialChars

randomIndex :: Int -> IO Int
randomIndex n = randomRIO (0, n - 1)

randomChar :: IO Char
randomChar = do
  index <- randomIndex (length allChars)
  return (allChars !! index)

generatePassword :: Int -> IO String
generatePassword lengthOfPassword = do
  mapM (const randomChar) [1 .. lengthOfPassword]

main :: IO ()
main = do
  putStrLn "Enter the length of the password:"
  input <- getLine
  let lengthOfPassword = read input :: Int
  password <- generatePassword lengthOfPassword
  putStrLn $ "Generated password: " ++ password
