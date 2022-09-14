{-# language TypeFamilies #-}

-- make a closed type class for event names

type class EventName (en :: Symbol) where


-- a list of all event names  https://www.w3schools.com/jsref/dom_obj_event.asp
-- includes types labelled "Belongs To".  each name can belong to several types.
-- Those types represent the event value that is produced.

-- There is another object type in play. The eventhandler has to be attached to something. This is known
-- as the EventTarget.  Not all objects can be eventtargets, and those that can be, only produce certain
-- event types.  I don't have a reference for that yet.

-- There is confusion about type names.  Some say the event type is the string used as an argument to event handler.
-- "mousedown" and "mouseup" are both event types.  If triggered, the event handler will receive an object.


foreach EventTarget et
   foreach EventName en
     foreach EventObject eo
         make et
         syntheasize event with secret code
         et.addEventListener(en, \e -> verify et eo (typeof e) secretCode)
         send event
         Display results as an image.
         
