-- convert Edifact message to Edifact XML
import Edifact

main = convertEdifact
    where
	convertEdifact =
		do
			s <- getContents
			let t = unLines s
			putStrLn $ ediXml (parseMessage t)
			where
				unLines = filter (\s -> (s /= '\n') && (s /= '\r'))