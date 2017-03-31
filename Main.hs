{-# LANGUAGE ScopedTypeVariables, FlexibleInstances, GeneralizedNewtypeDeriving, MultiParamTypeClasses, UndecidableInstances #-}

{-
  Copyright Massimo Zaniboni (c) 2010, 2017

All rights reserved.

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

    * Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

    * Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials provided
      with the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-}

module Main where

import Data.Maybe

import Control.Monad
import Control.Exception

import Text.Html

import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Data.Char

import System.Time
import System.Posix.Files
import System.IO
import qualified GHC.IO.Encoding as Encoding
import Text.Parsec
import Text.Parsec.Prim
import Text.Parsec.Language
import Text.Parsec.String

import Debug.Trace

www_out_directory = "web/"

palm_out_directory = "palm_web/"

page_home_url = "index.html"
page_atleti_url = "atleti.html"
page_gare_url = "gare.html"
page_hall_of_fame_url = "hall_of_fame.html"
page_tempi_personali_recenti_url = "tempi_personali_recenti.html"

page_title = "Sea Sub Nuoto Master"

-- | Quanti anni si va indietro nel passato
--   rispetto alla data corrente nella pagina Hall of Fame
--
howManyYearsInThePast = 3

-- | I dati in formato testuale
--
data_file = "apostovia.dat"
-- data_file = "temp.dat"

---------------------------------
-- HASKELL DATA REPRESENTATION --
---------------------------------

-- | A date in format "YYYY-MM-DD"
--
type UDate = String

type Id = Int

type Milliseconds = Int

data Team = Team {
   team_id :: Id
 , team_name :: String
 , team_place :: String
 } deriving (Eq, Ord, Show)

data Athlete = Athlete {
   athlete_id :: Id
 , athlete_teamId :: Id
 , athlete_name :: String
 , athlete_surname :: String
 , athlete_nickname ::  String
 , athlete_birthday :: UDate
 , athlete_subscribed :: Bool
 } deriving (Eq, Ord, Show)

data Discipline = Discipline {
   discipline_id :: Id
 , discipline_name :: String 
 , discipline_code :: String
 , discipline_ord :: Int 
 } deriving (Eq, Ord, Show)

data Event = Event {
   event_id :: Id
 , event_typeId :: Id
 , event_startDate :: UDate
 , event_name :: String 
 } deriving (Eq, Ord, Show)

data EventType = EventType {
   eventType_id :: Id
 , eventType_name :: String
 } deriving (Eq, Ord, Show)

data Race = Race {
   race_id :: Id
 , race_athleteId :: Id 
 , race_eventId :: Id 
 , race_disciplineId :: Id
 , race_repetitionDistance :: Int 
 , race_nrOfRepetitions :: Int 
 , race_milliseconds :: Milliseconds 
 , race_note :: String 
 , race_video :: Maybe String
 } deriving (Eq, Ord, Show)

race_distance:: Race -> Int
race_distance r 
  = (race_repetitionDistance r) * (race_nrOfRepetitions r)

data Db = Db {
  db_team :: Map.Map Id Team
 ,db_athlete :: Map.Map Id Athlete 
 ,db_discipline :: Map.Map Id Discipline
 ,db_event :: Map.Map Id Event
 ,db_eventType :: Map.Map Id EventType 
 ,db_race :: Map.Map Id Race
  }

-------------------------
-- FROM DAT FILE TO DB --
-------------------------

pTry = Text.Parsec.Prim.try

-- | Associa ad un identifier il suo ID.
--
type SymbolTable = Map.Map String Id

type DATParser a = GenParser Char (Id, SymbolTable) a

insertSymbol :: String -> DATParser  Id
insertSymbol name
  = do (currId, symbols) <- getState
       (newId, newSymbols) <- return (currId+1, Map.insert name (currId+1) symbols)
       setState(newId, newSymbols)
       return newId

currId :: DATParser Id
currId
  = do (c, _) <- getState
       return c

newId :: DATParser Id
newId
  = do (c, s) <- getState
       setState (c+1, s)
       currId

parseInt :: DATParser Int
parseInt = read <$> many1 digit

symbolId :: String -> DATParser Id
symbolId name
  = do (_, m) <- getState
       case Map.lookup name m of
         Just r  -> return r
         Nothing -> error ("symbol " ++ show name ++ "not present!")

-- | Esegue il parsing di apostovia.dat
--   Vedere il file per una "descrizione" effettiva del formato.
--
from_DAT_to_DB :: DATParser Db
from_DAT_to_DB
  = do eventTypes <- named_section "event_type" (id_sections event_type_record)
       teams <- named_section "team" (id_sections team_record)
       disciplines <- named_section "discipline" (id_sections discipline_record)
       athletes2 <- named_section "athletes" (many team_athletes)
       eventsWithRaces <- named_section "events" (many event_record)

       athletes :: [Athlete] <- return (concat athletes2)

       events :: [Event] <- return (map fst eventsWithRaces)
       races :: [Race] <- return (concatMap snd eventsWithRaces)

       spaces
       eof
       return (Db {
         db_team = Map.fromList (zip (map team_id teams) teams)
        ,db_athlete = Map.fromList (zip (map athlete_id athletes) athletes)
        ,db_discipline = Map.fromList (zip (map discipline_id disciplines) disciplines)
        ,db_event = Map.fromList (zip (map event_id events) events)
        ,db_eventType = Map.fromList (zip (map eventType_id eventTypes) eventTypes)
        ,db_race = Map.fromList (zip (map race_id races) races)
                  })

 where

  nl = many1 $ oneOf ['\n','\r']

  my_identifier :: DATParser String
  my_identifier 
    = do c  <- letter 
         cs <- many (alphaNum <|> char '_' <|> char '-') 
         return (c:cs)

  named_section :: String -> DATParser a -> DATParser a
  named_section sectionName subParser
    = do spaces
         pTry (string sectionName) <?> ("section name " ++ show sectionName)
         spaces
         string "{"
         spaces
         r <- subParser
         spaces
         string "}"
         spaces
         return r

  id_section :: DATParser a -> DATParser a
  id_section subSectionParser
    = do spaces
         name <- pTry my_identifier <?> "section identifier"
         id <- insertSymbol name 
         spaces
         string "{" <?> "section start {"
         r <- subSectionParser
         spaces
         string "}" <?> "section end }"
         spaces
         return r

  id_sections :: DATParser a -> DATParser [a]
  id_sections subParser
    = do many (id_section subParser) <?> "section"

  recordInt :: String -> DATParser Int
  recordInt name = do
    recordLabel name
    r <- parseInt
    spaces
    return r

  recordLabel :: String -> DATParser ()
  recordLabel name
    = do spaces
         pTry (string name) <?> ("Record " ++ show name ++ " is missing")
         char ':' <?> ("Record " ++ show name ++ " ':' separator is missing")
         many (char ' ')
         return ()
    
  record :: String -> DATParser String
  record name
    = do recordLabel name 
         r <- manyTill anyChar nl <?> ("Record " ++ show name ++ " value is missing")
         spaces
         return r

  recordDate :: String -> DATParser UDate
  recordDate name
    = do recordLabel name
         pTry readDate <|> readNullDate
   where

     readNullDate = do
         spaces
         return ""
  
     readDate = do
         year <- count 4 digit
         char '-'
         mounth <- count 2 digit
         char '-'
         day <- count 2 digit
         r <- manyTill anyChar nl <?> ("Record " ++ show name ++ " value is missing")
         return (year ++ "-" ++ mounth ++ "-" ++ day)

  event_type_record
    = do n <- record "name"
         id <- currId
         return (EventType { eventType_id = id, eventType_name = n })

  team_record
    = do name <- record "name"
         place <- record "place"
         id <- currId
         return (Team { team_id = id, team_name = name, team_place = place })

  discipline_record
    = do name <- record "name"
         code <- record "code"
         order <- recordInt "ord"
         id <- currId
         return (Discipline { 
                   discipline_id = id,
                   discipline_name = name, 
                   discipline_code = code, 
                   discipline_ord = order 
                            })

  team_athletes
    = do spaces
         team <- my_identifier <?> "team identifier"
         teamId <- currId
         spaces
         string "{"
         r <- id_sections (athlete_record teamId)
         spaces
         string "}"
         spaces
         return r

  athlete_record teamId
    = do name <- record "name"
         surname <- record "surname"
         nickname <- record "nickname"
         birthday <- recordDate "birthday"
         subscribed1 <- record "subscribed"

         subscribed <- case subscribed1 of
                         "1" -> return True
                         _   -> return False
         id <- currId
         return (Athlete {
           athlete_id = id
         , athlete_teamId = teamId
         , athlete_name = name
         , athlete_surname = surname
         , athlete_nickname = nickname 
         , athlete_birthday = birthday
         , athlete_subscribed = subscribed
         })

  event_record :: DATParser (Event, [Race])
  event_record 
    = do spaces
         eventName <- pTry my_identifier <?> "event identifier"
         eventId <- insertSymbol eventName
         spaces
         string "{" <?> "event section start {"

         e_type <- record "type"
         typeId <- symbolId e_type

         start_date <- record "start_date"
         name <- record "name"

         r1 <- return (Event {
            event_id = eventId
          , event_typeId = typeId
          , event_startDate = start_date
          , event_name = name})

         r2 <- many (named_section "race" (race_record eventId))

         spaces
         string "}" <?> "event section end }"
         spaces

         return (r1, concat r2)

  race_record :: Int -> DATParser [Race]
  race_record eventId
    = do disciplineType <- record "discipline"
         disciplineId <- symbolId disciplineType

         distance <- recordInt "distance"
         nr_of_repetitions <- recordInt "nr_of_repetitions"

         named_section "results" (many (result_record eventId disciplineId distance nr_of_repetitions))

  result_record :: Int -> Int -> Int -> Int -> DATParser Race
  result_record eventId disciplineId distance nr_of_repetitions
     = do spaces
          athlete <- my_identifier
          athleteId <- symbolId athlete

          spaces
          time <- my_time
          spaces
          maybeVideoURL <- Text.Parsec.option 
                             Nothing 
                             (do char '['
                                 r <- manyTill anyChar (char ']')
                                 return $ Just r)

          raceId <- newId
          spaces

          return (Race {
            race_id = raceId
          , race_athleteId = athleteId
          , race_eventId = eventId
          , race_disciplineId = disciplineId
          , race_repetitionDistance = distance
          , race_nrOfRepetitions = nr_of_repetitions
          , race_milliseconds = time
          , race_note = ""
          , race_video = maybeVideoURL })

  my_time
     = do spaces
          (m1, s1, c1) <- format1
          spaces

          m :: Int <- return (read m1)
          s :: Int <- return (read s1)
          c :: Int <- return (read c1)

          return (m*60*100+s*100+c)

    where
      format1
        = do m1 :: String <- pTry (manyTill digit (string "'" <|> string " ")) <|> (return "0")
             s1 :: String <- manyTill digit ((string "''") <|> string "\"" <|> string "." <|> string " ")
             c1 :: String <- many1 digit
             return (m1, s1, c1)


--------------------------
-- FROM DB TO WEB SITE  --
--------------------------

hotlink1:: URL -> Html -> HotLink
hotlink1 u h = hotlink u [h]

-- | Dato un tempo in centesimi lo visualizza
--   nel formato "1'0''456"
--
displayTempo :: Milliseconds -> String
displayTempo totMilliseconds
  = let -- NOTA: in realta` i milliseconds sono centesimi...
        totCentesimi = totMilliseconds
        totSeconds = div totCentesimi 100
        totMinutes = div totSeconds 60

        centesimi = rem totCentesimi 100
        seconds = rem totSeconds 60
        minutes = totMinutes

        myShow :: (Show a) => Int -> a -> String
        myShow totLen nr
          = let nr1 = show nr
            in  (replicate (totLen - length nr1) '0') ++ nr1

    in  show minutes ++ "'" ++ myShow 2 seconds ++ "''" ++ myShow 2 centesimi

displayDate :: String -> String
displayDate yyyy_mm_dd
  = let (yyyy, mm_dd_) = splitAt 4 yyyy_mm_dd
        (mm, dd_) = splitAt 2 (tail mm_dd_)
        dd = tail dd_
    in  dd ++ "/" ++ mm ++ "/" ++ yyyy 

displayEventName :: Int -> String -> String -> String -> Bool -> Html
displayEventName id name eventType startDate addEventType
  = toHtml (hotlink1 (event_url id name startDate)
                     (stringToHtml (displayDate startDate ++ " " ++ name ++
                                    (if addEventType then (" - " ++ eventType) else "")))
           )

displayAthleteName :: Id -> String -> String -> String -> Html
displayAthleteName id name surname nickname
  = toHtml (hotlink1 (athlete_url id surname name)
                     (stringToHtml (surname ++ " " ++ name ++ " " ++ nickname2)))
 where
   nickname2
     = if length (filter (\ c -> not (c == ' ')) nickname) == 0
       then ""
       else (" (" ++ nickname ++ ")")

event_url :: Id -> String -> String -> String
event_url id place date
  = "__" ++ urlificator place ++ "_" ++ urlificator date ++ ".html"

athlete_url :: Id -> String -> String -> String
athlete_url id surname name
  = "_" ++ urlificator surname ++ "_" ++ urlificator name ++ ".html"

urlificator :: String -> String
urlificator s
  = map f s
 where
  f c = if isAlpha c || isDigit c
        then c
        else '_'

deurlificator :: String -> String
deurlificator s
  = let s2 = map f s
        (s3, s4) = splitAt (length s2 - 5) s2
    in  if s4 == ".html"
        then s3
        else s2
 where
  f c = if c == '_'
        then ' '
        else c

-- | Torna l'evento piu` recente.
--
getLastEvent :: Db -> Event
getLastEvent db
 = let events = db_event db
       f e1 Nothing = Just e1
       f e1 (Just e2) = if event_startDate e1 > event_startDate e2
                        then Just e1
                        else Just e2

       result = fromJust (Map.fold f Nothing events)

   in  result

getEventName :: Db -> Id -> Bool -> Html
getEventName db id addEventType
 = let events = db_event db
       event = fromJust (Map.lookup id events)
       eventType = fromJust (Map.lookup (event_typeId event) (db_eventType db))
   in  displayEventName id (event_name event) (eventType_name eventType) (event_startDate event) addEventType
 
getAthleteName :: Db -> Id -> Html
getAthleteName db id
 = let x = fromJust (Map.lookup id (db_athlete db))
   in  displayAthleteName id (athlete_name x)  (athlete_surname x) (athlete_nickname x)

-- | Describe a Race Type.
-- 
data RaceType
  = RaceType {
      raceType_disciplineId :: Id
     ,raceType_distance :: Int
     ,raceType_repetitionDistance :: Int
             }
 deriving (Eq, Ord, Show)

raceType_disciplineAndDistanceChange:: RaceType -> RaceType -> Bool
raceType_disciplineAndDistanceChange r1 r2
  = if (raceType_disciplineId r1) /= (raceType_disciplineId r2)
    then True
    else if (raceType_distance r1) /= (raceType_distance r2)
         then True
         else False

type AthleteId = Id

-- | The best race description for every Athlete.
--
type AthleteBestRace = Map.Map (AthleteId, RaceType) (Html, Milliseconds)

-- | Only races before this date are considered.
--   This permits to isolate the previous best race record.
--
type DateLimit = UDate

-- | Only races before the specified date are considered.
--
calcBestRaceResults :: Db -> DateLimit -> AthleteBestRace
calcBestRaceResults db dateLimit 

 = foldl f Map.empty startingRaces 

 where

  event r = fromJust (Map.lookup (race_eventId r) (db_event db))

  isOk r = ((event_startDate (event r)) < dateLimit)
  
  allRaces = (Map.elems (db_race db))

  startingRaces :: [Race]
  startingRaces = (filter isOk allRaces)

  f :: AthleteBestRace -> Race -> AthleteBestRace
  f bestRaces race 
    = let raceType
            = RaceType {
                raceType_disciplineId = (race_disciplineId race)
              , raceType_distance = (race_distance race)
              , raceType_repetitionDistance = (race_repetitionDistance race)
                       }

          tempo = stringToHtml (displayTempo (race_milliseconds race))

          event_full_name = getEventName db (race_eventId race) True

          description = (tempo +++ stringToHtml " - " +++ event_full_name)

          maybeNewBestRaces :: AthleteBestRace
          maybeNewBestRaces
            = Map.insert (race_athleteId race, raceType) (description, race_milliseconds race) bestRaces

          newBestRaces
            = case Map.lookup (race_athleteId race, raceType) bestRaces of
                Just (oldHtml, oldBestMilliseconds)
                  -> if (race_milliseconds race < oldBestMilliseconds)
                     then maybeNewBestRaces 
                     else bestRaces

                Nothing
                  -> maybeNewBestRaces

      in newBestRaces

-- | Mostra le gare in ordine inverso di svolgimento
--   in forma "compatta". Se si clicka sulla gara si salta
--   alla pagina corrispondente.
--
generateListOfEvents :: Db -> Bool -> Html
generateListOfEvents db addEventType
 = let events = Map.elems (db_event db)
       f e1 e2 = compare (event_startDate e2) (event_startDate e1)

       sortedEvents = sortBy f events

       addEvent r e 
         = r +++ li << (getEventName db (event_id e) addEventType)

       allEvents = foldl addEvent noHtml sortedEvents

   in  paragraph << (stringToHtml "Gare: " +++ (ulist << allEvents))

generateListOfConcorrenti :: Db -> Html
generateListOfConcorrenti db
 = let athletes = Map.elems (db_athlete db)
       f e1 e2 
         = compare (athlete_surname e1 ++ " " ++ athlete_name e1) 
                   (athlete_surname e2 ++ " " ++ athlete_name e2)

       sortedAthletes = sortBy f athletes

       addAthlete r e 
         = r +++ li << (getAthleteName db (athlete_id e))

       allAthletes isSubscribed 
         = foldl addAthlete noHtml (filter (\e -> (athlete_subscribed e) == isSubscribed) sortedAthletes)

   in  (paragraph << (stringToHtml "Atleti tesserati: " +++ (ulist << allAthletes True)))
        +++ (paragraph << (stringToHtml "Vecchi tesserati: ") +++ allAthletes False)

-- | A filter condition on Race Results generation.
--
data FilterCond
  = EventFilterCond Int DateLimit
  | AthleteFilterCond Int
  | HallOfFameCond
  -- ^ include anche gli atleti non piu` iscritti
  --   e non pone nessun limite sugli anni...
  | AthleteComparisonsCond Int
  -- ^ indica l'anno limite prima del quale
  --   non includere piu` risultati...

-- | Return a page in WWW and PALM WWW format
--
generateRaceResults :: Db -> FilterCond -> (Html, Html)
generateRaceResults db filterCond
 = (table1, palm)

 where

  table1 = ((table << (tableCaption +++ tableContent)) ! [theclass "sf_admin_list"])

  maybeBestRaces 
    = case filterCond of
        EventFilterCond id limitDate
          -> Just (calcBestRaceResults db limitDate)
        AthleteFilterCond _
          -> Nothing
        HallOfFameCond
          -> Nothing
        AthleteComparisonsCond _
          -> Nothing

  tableCaptionText
    = case filterCond of
        EventFilterCond id limitDate
          -> getEventName db id True
        AthleteFilterCond id
          -> getAthleteName db id
        HallOfFameCond
          -> (stringToHtml "Risultati migliori di tutti i tempi (Hall of Fame)")
        AthleteComparisonsCond limitYears
          -> (stringToHtml ("I risultati migliori dal " ++ show limitYears ++ " ad oggi degli atleti tesserati"))

  tableCaption = caption tableCaptionText

  nullRaceType
    = RaceType {
        raceType_disciplineId = -1
       ,raceType_distance = 0
       ,raceType_repetitionDistance = 0
               }

  isOk r= case filterCond of
            EventFilterCond id limitDate
              -> race_eventId r == id
            AthleteFilterCond id
              -> race_athleteId r == id
            HallOfFameCond
              -> True
            AthleteComparisonsCond nrOfYears
              -> athlete_subscribed (athlete r)


  races = filter isOk (Map.elems (db_race db))

  sortOnField [] r1 r2 = EQ
  sortOnField (f1:fr) r1 r2 
    = let r0 = compare (f1 r1) (f1 r2)
      in  if r0 == EQ
          then sortOnField fr r1 r2
          else r0
    
  completeSort r1 r2
    = case sortOnField [\r -> discipline_ord (discipline r),
                        race_distance,
                        race_repetitionDistance,
                        race_milliseconds] r1 r2 of
        EQ -> compare (event_startDate (event r1)) (event_startDate (event r2))
        res -> res

  orderedRaces = sortBy completeSort races

  -- | Return an anchor for a certain race type
  --
  raceAnchorName:: Race -> String
  raceAnchorName r 
    =    ""  
      ++ (show $ race_disciplineId r)
      ++ "_"
      ++ (show $ race_nrOfRepetitions r)
      ++ "x"
      ++ (show $ race_repetitionDistance r)

  raceAnchorSource:: Race -> Html -> Html
  raceAnchorSource r h = (anchor h) ! [name $ raceAnchorName r]

  raceAnchorJump:: Race -> Html -> Html
  raceAnchorJump r h 
    = anchor h ! [href ("#" ++ (raceAnchorName r))]

  palm = ((h1 << tableCaptionText)
          +++ palmUL
          +++ (ulist << palmLI)
         )

  event :: Race -> Event
  event r = fromJust (Map.lookup (race_eventId r) (db_event db))

  eventType r = fromJust (Map.lookup (event_typeId (event r)) (db_eventType db))

  athlete r = fromJust (Map.lookup (race_athleteId r) (db_athlete db))

  discipline :: Race -> Discipline
  discipline r = fromJust (Map.lookup (race_disciplineId r) (db_discipline db))

  (tableHeader, colSpan)
    = let tableHeader1
             = (tr << ((th << stringToHtml "Tempo")
                        +++ (th << stringToHtml "Atleta")
                        +++ (th << stringToHtml "Record Personale Precedente")
                        +++ (th << stringToHtml "Filmato Gara"))
               )

          tableHeader2
            = (tr << ((th << stringToHtml "Tempo")
                       +++ (th << stringToHtml "Data e Luogo")
                       +++ (th << stringToHtml "Media Vasca")
                       +++ (th << stringToHtml "Filmato Gara"))
              )

      in case filterCond of
           EventFilterCond id limitDate
            -> (tableHeader1, colspan 2)
           AthleteFilterCond id
            -> (tableHeader2, colspan 2)
           HallOfFameCond
            -> (tableHeader2, colspan 2)
           AthleteComparisonsCond nrOfYears
            -> (tableHeader2, colspan 2)

  (tableContent, palmUL, palmLI, nrOfRecors, _, _, _) 
    = foldl (addResult maybeBestRaces) (noHtml, noHtml, noHtml, 0, nullRaceType, 0, Set.empty) orderedRaces

  -- | Mostra un HEADER diverso ogni volta che si cambia 
  --   intestazione (stile - distanza - nr. ripetitioni)
  --
  addResult maybeBestRaces (r0, palmUL0, palmLI0, nrOfRecors, lastHeader, rowNr, insertedAthleteIds0) r
    = (r2, palmUL2, palmLI2, nrOfRecords2, currHeader, newRowNr, insertedAthleteIds2)
   where

    milliseconds1  = race_milliseconds r
    repetition_distance1 = race_repetitionDistance r
    distance1 = race_distance r
    nr_of_repetitions1 = race_nrOfRepetitions r
    athlete_surname1 = (athlete_surname (athlete r))
    athlete_name1 = (athlete_name (athlete r))
    athlete_nickname1 = (athlete_nickname (athlete r))
    athlete_birthday1 = (athlete_birthday (athlete r))
    discipline_name1 = (discipline_name (discipline r))
    discipline_code1 = (discipline_code (discipline r))
    event_type_name1 = (eventType_name (eventType r))
    event_start_date1 :: UDate = (event_startDate (event r))
    event_name1 = (event_name (event r))
    discipline_id1 = (race_disciplineId r)
    athlete_id1  = (race_athleteId r)
    event_id1 = (race_eventId r)
    race_note1 = (race_note r)
                
    event_year1 :: Int =  (read (take 4 event_start_date1))
    currHeader = RaceType {
                   raceType_disciplineId = discipline_id1
                  ,raceType_distance = distance1
                  ,raceType_repetitionDistance = repetition_distance1
                 }


    -- Menu HEADER, something like:
    --
    -- > |stile| delfino, stile, dorso, rana|
    -- > |distanza| 100m, 200m, 400m|
    -- > |vasca| 25m 50m|
    --
    -- Every current selection is in bold.
    --
    -- A click on "distanza 200m" is a jump to 
    -- the first race of the selected style
    -- and the "vasca 25m". Then a selection of
    -- "vasca 50m" is a jump to the same type of
    -- race but with a different vasca lenght.
    --
    menu:: Race -> Html 
    menu race
      = let 
            -- if you change this code you must change also "orderedRaces" criteria.
            --
            racesByDiscipline:: [[Race]]
              = groupBy (\r1 r2 -> (race_disciplineId r1) == (race_disciplineId r2)) orderedRaces

            currentRacesByDistance:: [[Race]]
              = let rs = filter (\r -> race_disciplineId r == discipline_id1) orderedRaces
                in groupBy (\r1 r2 -> (race_distance r1) == (race_distance r2) 
                                      && (race_repetitionDistance r1) == (race_repetitionDistance r2)) rs

            formatSelection:: Html -> Html
            formatSelection h
              = (thespan $ bold h) ! [(thestyle "text-decoration: underline; font-size: large;")]

            menuOfDisciplines:: Html
              = let g1 r = stringToHtml $ (discipline_name (discipline r)) ++ "   "
                    g2 r = if race_disciplineId r == discipline_id1
                           then formatSelection $ g1 r
                           else g1 r
                    h r1 r = r1 +++ (raceAnchorJump r (g2 r))
                in  foldl h noHtml (map head racesByDiscipline)

            menuOfDistances:: Html
              = let g1 r = stringToHtml $ show (race_distance r) 
                                          ++ "m (" 
                                          ++ show (race_nrOfRepetitions r) 
                                          ++ "x" ++ show (race_repetitionDistance r)
                                          ++ ")   "
                    g2 r = if race_distance r == distance1
                              && race_repetitionDistance r == repetition_distance1
                           then formatSelection $ g1 r
                           else g1 r
                    h r1 r = r1 +++ (raceAnchorJump r (g2 r))
                in  foldl h noHtml (map head currentRacesByDistance)

            c11 = td << stringToHtml ""
            c12 = (td << (raceAnchorSource r menuOfDisciplines)) ! [colSpan]
            c21 = c11
            c22 = (td << menuOfDistances) ! [colSpan]

            r1 = (tr << (c11 +++ c12 +++ c11)) ! [theclass "sf_menu_row_header"]
            r2 = (tr << (c21 +++ c22 +++ c11)) ! [theclass "sf_menu_row_header"]

        in  r1 +++ r2

    -- Inserisco l'header se cambia stile
    --
    (r1, palmUL1, palmLI1, insertedAthleteIds1, newRowNr0)
       = if (currHeader == lastHeader)
         then (r0, palmUL0, palmLI0, insertedAthleteIds0, rowNr)
         else let   description
                      = show (nr_of_repetitions1 * repetition_distance1)
                        ++ " " ++ discipline_name1
                        ++ " - vasca " ++ show (repetition_distance1) ++ " metri"

                    description2
                      = discipline_name1
                        ++ " - "
                        ++ show distance1
                        ++ "m (" 
                        ++ show nr_of_repetitions1
                        ++ "x" ++ show repetition_distance1
                        ++ ") "

                    newHeaderContent
                      = (td << stringToHtml description2) ! [colSpan]

                    newHeaderRow
                      = (menu r)
                        +++ ((tr << ((td << stringToHtml "") +++ newHeaderContent))
                             ! [theclass "sf_admin_row_header"])
                        +++ tableHeader

                    newHeaderRow2 = menu r +++ tableHeader

                    newPalmUL
                      = palmUL0 +++ (ulist << palmLI0) +++ (h2 << stringToHtml description)

              in  (r0 +++ newHeaderRow2, newPalmUL, noHtml, Set.empty, 0)

    isNewRow
      = case filterCond of
                   EventFilterCond id _
                     -> True
                   AthleteFilterCond id
                     -> True
                   HallOfFameCond
                     -> (Set.notMember athlete_id1 insertedAthleteIds1)
                   AthleteComparisonsCond limitYear
                     -> (Set.notMember athlete_id1 insertedAthleteIds1 && event_year1 >= limitYear)

    insertedAthleteIds2
              = if isNewRow
                then (Set.insert athlete_id1 insertedAthleteIds1)
                else insertedAthleteIds1

    newRowNr =  if isNewRow
                then (newRowNr0 + 1)
                else newRowNr0

    cellAttr =  if (rem newRowNr 2) == 0
                then  ([theclass "sf_admin_row_1"])
                else  ([theclass "sf_admin_row_0"])

    tempo = (stringToHtml (displayTempo milliseconds1))

    athlete2 = (displayAthleteName athlete_id1 athlete_name1 athlete_surname1 athlete_nickname1)

    event2 = (displayEventName event_id1 event_name1 event_type_name1 event_start_date1 True)

    athlete_and_event2 = (athlete2 +++ stringToHtml " - " +++ event2)

    condRow 
      = case filterCond of
          EventFilterCond id _
            ->  athlete2
          AthleteFilterCond id
            ->  event2
          HallOfFameCond
            ->  athlete_and_event2
          AthleteComparisonsCond _
            ->  athlete_and_event2

    (bestRaceCell, palmBestRace)
               = case maybeBestRaces of
                    Nothing
                      -> let milliPerVasca
                               = displayTempo (div milliseconds1 nr_of_repetitions1)
                                 ++ "x" ++ show nr_of_repetitions1
                                 ++ "x" ++ show repetition_distance1 ++ "m"

                             note
                               = milliPerVasca
                                 ++ (if race_note1 == "" then "" else (" - " ++ race_note1))

                         in  ((td << (stringToHtml note)),
                              (ulist << (li << (stringToHtml note)))
                             )
                    Just bestRaces
                      -> case Map.lookup (athlete_id1, currHeader) bestRaces of
                           Nothing
                             -> ((td << noHtml), (ulist << (li << (stringToHtml "prima gara di questo tipo"))))
                               
                           Just (descr, _)
                             -> ((td << descr), (ulist << (li << descr)))

    linkToVideo
      = case race_video r of
          Nothing
            -> stringToHtml ""
          Just urlVideo
            -> (anchor ! [href urlVideo, target "_blank"]) << stringToHtml "filmato della gara"

    (newRow, palmUL2, palmLI2)
              =  ((tr << ((td << tempo) +++ (td << condRow) +++ bestRaceCell +++ (td << linkToVideo))) ! cellAttr,
                  palmUL1,
                  palmLI1 +++ (li << (tempo +++ stringToHtml " "
                                      +++ condRow +++  palmBestRace ))
                 )

    (r2,nrOfRecords2)
              =   if isNewRow
                  then  (r1 +++ newRow, nrOfRecors+1)
                  else  (r1, nrOfRecors)


-- | Force the creation of a page for each Athlete.
--
generateAllAthletePages db
  = do  allAthletes <- return (generateListOfConcorrenti db)
        athleteIds <-  return (Map.keys (db_athlete db))
        mapM_ (generatePage allAthletes) athleteIds
        return ()
 where

  generatePage allAthletes athleteId
      = do  athlete <- return (fromJust (Map.lookup athleteId (db_athlete db)))
            (wwwPageContent, palmPageContent) <- return (generateRaceResults db (AthleteFilterCond athleteId))
            pageName <- return (athlete_url athleteId (athlete_surname athlete) (athlete_name athlete))
            createPage allAthletes pageName pageName (wwwPageContent, palmPageContent)
            return ()

-- | Force the creation of a HTML page for each Event.
--
generateAllEventsPages db
  = do  allEvents <- return (generateListOfEvents db False)
        ids <-  return (Map.keys (db_event db))
        mapM_ (generatePage allEvents) ids
        return ()
 where

  generatePage allEvents id
      = do  e <- return (fromJust (Map.lookup id (db_event db)))

            (wwwPageContent, palmPageContent) 
              <- return (generateRaceResults db (EventFilterCond id (event_startDate e)))

            pageName <- return (event_url id (event_name e) (event_startDate e))
            createPage allEvents pageName pageName (wwwPageContent, palmPageContent)
            return ()


-- | Create a standard page with the propers header, footer,
--   menu etc..
--
--   Generate according http://www.pmob.co.uk/temp/2colcentred_contentfirst.htm
--
-- Generate both WEB format and compressed PALM HTML format.
--
createPage :: Html -> String -> String -> (Html, Html) -> IO ()
createPage leftMenu fileName title (wwwContent, palmContent)
  = do normal_html
       palm_html
       return ()
 where

  writeUtf8File fileName content = do
       h <- openFile fileName WriteMode
       hSetEncoding h utf8
       hPutStr h content
       hClose h

  normal_html = do 
       headerLogoDiv
         <- return ((thediv ! [identifier "header-logo"]) <<
                     (hotlink1 "index.html" (image ! [src "resources/logo_small.png", alt "Sea Sub Modena"])))

       headerDescriptionDiv
         <- return ((thediv ! [identifier "header-description"]) << linesToHtml ["Sea Sub Modena", "Settore Nuoto Master", "Tempi Gare"])

       headerTitleDiv
         <- return ((thediv ! [identifier "header-title"]) <<
                     (hotlink1 "index.html" (image ! [src "resources/apostovia_small.png", alt "A POSTO...VIA!"])
                     ))

       headerDiv
         <- return ((thediv ! [identifier "header-container"]) << (headerLogoDiv +++ headerDescriptionDiv +++ headerTitleDiv))

       today <- getClockTime
       today <- toCalendarTime today
       footerContent
         <- return ((paragraph <<
                        (stringToHtml (
                            " il sito e` stato aggiornato il "
                            ++ show (ctDay today) ++ "-" ++ show (ctMonth today) ++ "-" ++ show (ctYear today)
                            ++ " utilizzando ")
                        +++ (hotlink1 "https://github.com/massimo-zaniboni/apostovia"
                                     (stringToHtml "https://github.com/massimo-zaniboni/apostovia")))
                    ) +++
                    (paragraph <<
                        (stringToHtml "webmaster: "
                        +++ hotlink1 "mailto:massimo.zaniboni@gmail.com"
                                     (stringToHtml "massimo.zaniboni@gmail.com"))
                   ))

       footerDiv
         <- return ((thediv ! [identifier "footer"]) << footerContent)

       standardMenu
         <- return (paragraph << (ulist << ((li << hotlink1 page_home_url (stringToHtml "Home"))
                                             +++ (li << hotlink1 page_atleti_url  (stringToHtml "Elenco Atleti"))
                                             +++ (li << hotlink1 page_gare_url  (stringToHtml "Elenco Gare"))
                                             +++ (li << hotlink1 page_tempi_personali_recenti_url (stringToHtml "Valori in Acqua"))
                                             +++ (li << hotlink1 page_hall_of_fame_url  (stringToHtml "Hall Of Fame")))))

       leftDiv <- return ((thediv ! [identifier "sidebar"]) << ( standardMenu +++ leftMenu))
       contentDiv <- return ((thediv ! [identifier "content"]) << wwwContent)

       googleAnalyticsScript
         <- return "<script type=\"text/javascript\">\nvar gaJsHost = ((\"https:\" ==document.location.protocol) ? \"https://ssl.\" : \"http://www.\");\ndocument.write(unescape(\"%3Cscript src='\" + gaJsHost +\"google-analytics.com/ga.js' type='text/javascript'%3E%3C/script%3E\"));\n</script>\n<script type=\"text/javascript\">\nvar pageTracker = _gat._getTracker(\"UA-3624268-1\");\npageTracker._initData();\npageTracker._trackPageview();\n</script>"

       googleAnalytics
         <- return (primHtml googleAnalyticsScript)

       bodySection <- return (
         (thediv ! [identifier "container"]) 
         << (headerDiv +++ leftDiv +++ contentDiv +++ footerDiv +++ googleAnalytics))

       cssLink <- return (thelink noHtml ! [rel "stylesheet", thetype "text/css", href "resources/main.css"])

       page <- return (header << (thetitle << stringToHtml (page_title ++ " - " ++ deurlificator title) +++ cssLink) +++ body << bodySection)

       fileName <- return (www_out_directory ++ fileName)
       writeUtf8File fileName (renderHtml page)
       
  palm_html = do
       standardMenu
         <- return (paragraph
                      << (    hotlink1 page_atleti_url  (stringToHtml "[Atleti]")
                          +++ stringToHtml "  "
                          +++ hotlink1 page_gare_url  (stringToHtml "[Gare]")
                          +++ stringToHtml "  "
                          +++ hotlink1 page_tempi_personali_recenti_url (stringToHtml "[Valori in Acqua]")
                          +++ stringToHtml "  "
                          +++ hotlink1 page_hall_of_fame_url  (stringToHtml "[Hall Of Fame]")
                         )
                    )

       bodySection <- return (standardMenu +++ palmContent +++ standardMenu)
       cssLink <- return (thelink noHtml ! [rel "stylesheet", thetype "text/css", href "resources/main.css"])
       page <- return (header << (thetitle << stringToHtml title +++ cssLink) +++ body << bodySection)

       fileName <- return (palm_out_directory ++ fileName)
       writeUtf8File fileName (renderHtml page)
       putStr "+"

    

----------
-- MAIN --
----------

main = do
      Encoding.setLocaleEncoding utf8
      Encoding.setFileSystemEncoding utf8
      Encoding.setForeignEncoding utf8
      putStrLn "Parsing of Data Files"
      h <- openFile data_file ReadMode
      hSetEncoding h utf8
      dataContent <- hGetContents h
      db
        <- case (runParser from_DAT_to_DB (1, Map.empty) data_file dataContent) of
             Left e
               -> do error("Error with " ++ data_file ++ " " ++ show e)

             Right db_commands
               -> do return db_commands

      putStrLn "\nGenerating Web Site (WWW and PALM)"
      today <- getClockTime
      today <- toCalendarTime today
      yearLimit <- return ((ctYear today) - howManyYearsInThePast)

      putStrLn "\nGenerating List of Events"
      event_menu <- return (generateListOfEvents db False)
      complete_event_menu <- return (generateListOfEvents db True)

      putStrLn "\nGenerating List of Concorrenti"
      concorrenti_menu <- return (generateListOfConcorrenti db)

      putStrLn "\nGenerating Event Pages"
      generateAllEventsPages db

      putStrLn "\nGeneratin Athlete Pages"
      generateAllAthletePages db

      putStrLn "\nGeneratin Other Pages"

      lastEvent <- return (getLastEvent db)
      last_event_page <- return (generateRaceResults db (EventFilterCond (event_id lastEvent) (event_startDate lastEvent)))

      createPage event_menu page_home_url "Home" last_event_page

      hall_of_fame <- return (generateRaceResults db HallOfFameCond)
      createPage concorrenti_menu page_hall_of_fame_url "Hall of Fame" hall_of_fame

      recent_records <- return (generateRaceResults db (AthleteComparisonsCond yearLimit))
      createPage concorrenti_menu page_tempi_personali_recenti_url "Valori in Acqua" recent_records

      createPage concorrenti_menu page_atleti_url "Elenco Atleti" (((thediv concorrenti_menu) ! [identifier "content_list"]), concorrenti_menu)

      createPage event_menu page_gare_url "Elenco Gare" (((thediv complete_event_menu) ! [identifier "content_list"]), complete_event_menu)

      putStrLn ("WWW Web Site was generated on " ++ www_out_directory ++ "index.html")
      putStrLn ("PALM Web Site was generated on " ++ palm_out_directory ++ "index.html")

      return ()
