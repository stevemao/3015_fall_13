 module Lists where               
 
 unique [] = []               
 unique (h:t) = if (elem h t) then (unique t ) else h:(unique t)               
                
 remove_all x = filter (not . (==x))               
