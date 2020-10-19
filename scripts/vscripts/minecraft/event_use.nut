
::CapturedPlayer <- null

::AssignUserID <- function()
{
	local params = this.event_data
	if (::CapturedPlayer != null && ::CapturedPlayer.ValidateScriptScope())
	{
		local scope = ::CapturedPlayer.GetScriptScope()
		scope.userid <- params.userid
		::CapturedPlayer = null
	}
}

if (!("gameevents_proxy" in getroottable()) || !(::gameevents_proxy.IsValid()))
{
	::gameevents_proxy <- Entities.CreateByClassname("info_game_event_proxy")
	::gameevents_proxy.__KeyValueFromString("event_name", "player_use")
	::gameevents_proxy.__KeyValueFromInt("range", 0)
}

Think <- function()
{
	ply <- null
	while ((ply = Entities.FindByClassname(ply, "*")) != null)
	{
		if ((ply.GetClassname() == "player" || ply.GetClassname() == "bot") && ply.ValidateScriptScope())
		{
			local script_scope = ply.GetScriptScope()
			if (!("userid" in script_scope) && !("attemptogenerateuserid" in script_scope))
			{
				script_scope.attemptogenerateuserid <- true
				::CapturedPlayer = ply
				EntFireByHandle(::gameevents_proxy, "GenerateGameEvent", "", 0.0, ply, null)
				return
			}
		}
	}
}
