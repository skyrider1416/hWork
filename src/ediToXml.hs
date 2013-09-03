-- convert Edifact message to Edifact XML
import Edifact

main = interact (ediXml . parseMessage . unLines)
    where
		unLines = filter (\s -> (s /= '\n') && (s /= '\r'))
	
			