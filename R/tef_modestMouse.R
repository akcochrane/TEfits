#' Print a line from a Modest Mouse song.
#'
#' Mostly from This Is a Long Drive for Someone with Nothing to Think About
#'
#' @export
#'
tef_modestMouse <- function(){
  quotes <- c(
    'I push things out through my mouth, I get refilled through my ears.'
    ,'Henry, you danced like a wooden Indian.'
    ,'A nice heart and a white suit and a baby blue sedan
      And I am doing the best that I can.'
    ,'You bleached your hair, and you pawned your skis, and then you sold out for the shape of the palm tree scene.'
    ,"I've said what I'd said and you know what I mean, but I still can't focus on anything."
    ,'We kiss on the mouth but still cough down our sleeves.'
    ,'Got to go to work, got to go to work, got to have a job.'
    ,"The custom concern for the people
Build up the monuments and steeples to wear out our eyes."
    ,'I get up just about noon. My head sends a message for me to reach for my shoes, and then walk.'
    ,"This'll never end, this'll never end, this'll never stop."
    ,'It was well intentioned, but bad advice.'
    ,'I got one two three four five, six, six, six.'
    ,'Flourescent lightbulbs will make an absense of dark, but the light just aint there still, and she said...'
    ,'The real lights can make you heavy, but never ever really empty.'
    ,"I don't know so I don't bother."
    ,"Remember through sounds
Remember through smells
Remember through colors
Remember through towns
With fear and fascination
On what was here and what's replacing them now"
    ,"More housing developments go up
Named after the things they replace"
    ,"it all seems a little abrupt; no, I don't like this change of pace."
    ,"Build bridges to nothing, you'll get nowhere."
    ,"I don't feel and it feels fine"
    ,"Every planned occupation
Surefire disappointment up ahead
    Until they treat you like desert
    See mirages of friendship
    Face turns red"
    ,"Childhood's what makes you"
    ,"Weigh those opinions
More like air than lead"
    ,"Truly lonely; this place is flatter than it seems"
    ,"Pulled the scabs off of regrets; we haven't learned to eat our conscience yet"
    ,"I'm upset and I leave the door open wide
Our hearts are used up, cracked and dry"
    ,"P.S. There's a lot going on underneath
There's roots, there's pipes and there's drainage leaks"
    ,"Calmly crashing: I pace and I figure it out again."
    ,"One hand clapping; awake but napping."
    ,"Rows of lights to illuminate lines... Why don't they turn them off and let us see night?"
    ,"You don't even actually exist, so I just started shaking."
    ,"Standing looking at a photograph that you do not remember being taken."
    ,"I blame this town, this job, these friends... The truth is it's myself, and I'm trying to understand myself and pinpoint who I am."
    ,"Changed my mind so much I can't even trust it... My mind changed me so much I can't even trust myself."
    ,"I'm not sure who I am but I know who I've been."
    ,"I'd hate to see anybody fail. But I'd like to see you fail seeing me fail, though."
    ,"I said, 'You shouldn't make facts out of opinions.' He said that I was right, you're right, I knew that I was."
    ,"I said, 'You can't make everybody happy. 'He said, 'You'd like to at least make yourself happy, though.'"
    ,"Been there a half an hour, I wanna come home soon."
    ,"Now she doesn't feel lonely but she'd just as soon try."
    ,"They gave her a mirror so she could talk to a face. She still got plenty lonely but that's just the case with time."
    ,"Sometimes my feelings get in the way
Of what I really feel I needed to say"
    ,"We're all so funny but he's lost the joke now; our communications come in one lined jokes."
    ,"I made my shoes shine with black coal; but the polish didn't shine the hole."

     )

  cat(quotes[sample(length(quotes),1)])
}
