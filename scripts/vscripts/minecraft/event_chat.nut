
::GiveWeapon <- function(player, weapon)
{
	::game_player_equip <- Entities.CreateByClassname("game_player_equip")
	::game_player_equip.__KeyValueFromInt("spawnflags", 5)
	::game_player_equip.__KeyValueFromInt(weapon, 999999)
	EntFireByHandle(::game_player_equip, "Use", "", 0.0, player, null)
	::game_player_equip.__KeyValueFromInt(weapon, 0)
	::game_player_equip.Destroy()
}

::GetPlayerFromUserID <- function(id)
{
	found <- null
	while ((found = Entities.FindByClassname(found, "player")) != null)
	{
		if (found.ValidateScriptScope())
		{
			local scope = found.GetScriptScope()
			if ("userid" in scope && scope.userid == id)
			{
				return found
			}
		}
	}
	return null
}

::HelpList <- [
	"/spawn - teleport back to spawn",
	"/give - give yourself an item",
	"/points - see how many points you have",
	"/locate - reveal nearby dungeons",
	"/seed - show the world seed",
	"/help - show this message",
]

::CheckChat <- function()
{
	local data = this.event_data
	local ply = GetPlayerFromUserID(data.userid)
	local text = data.text
	local args = split(text, " ")
	if (text == "/help")
	{
		foreach (tip in HelpList)
		{
			ScriptPrintMessageChatAll(tip)
		}
	}
	if (text == "/spawn")
	{
		TeleportUp(ply)
		CenterPrint(ply, "Teleporting to spawn...")
	}
	if (text == "/seed")
	{
		ScriptPrintMessageChatAll("World seed: " + WORLD_SEED.tostring())
	}
	if (text == "/locate")
	{
		ScriptPrintMessageChatAll("Locating dungeons...")
		RevealAllDungeons()
	}
	if (text == "/points" && ply.ValidateScriptScope())
	{
		local scope = ply.GetScriptScope()
		if (!("points" in scope))
		{
			scope.points <- 0
			CenterPrint(ply, "You don't have any points!")
		}
		else
		{
			CenterPrint(ply, "You have " + scope.points + " point" + (scope.points == 1 ? "" : "s") + ".")
		}
	}
	if (args[0] == "/give" && typeof args[1] == "string")
	{
		GiveWeapon(ply, "weapon_" + args[1])
	}
}
