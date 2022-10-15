# get decade from date
floor_decade = function(value){ return(value - value %% 10) }
floor_timestep = function(value, step){return(value - value %% step)}
