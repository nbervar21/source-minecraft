
ScriptPrintMessageChatAll("Welcome to my Minecraft server!")
::WORLD_SEED <- RandomInt(1000000, 9999999)
srand(WORLD_SEED) // for testing: 2570233
ScriptPrintMessageChatAll("The seed for this world is " + WORLD_SEED.tostring())
ScriptPrintMessageChatAll("Type \"/help\" for a list of commands.")

SendToConsoleServer("sv_infinite_ammo 2")
SendToConsoleServer("mp_solid_teammates 0")
SendToConsoleServer("mp_respawn_on_death_t 1")
SendToConsoleServer("mp_respawn_on_death_ct 1")
SendToConsoleServer("mp_ignore_round_win_conditions 1")

::BlockMakers <- EntityGroup

::MAX_BLOCKS <- 1300

::BLOCK_WIDTH <- 56
::BLOCK_HEIGHT <- 56
::DIGSITE_WIDTH <- 20
::DIGSITE_HEIGHT <- 20

::BLOCKSTATE_PENDING <- 0
::BLOCKSTATE_OCCUPIED <- 1
::BLOCKSTATE_BROKEN <- 2

::BLOCKTYPE_GRASS <- 0
::BLOCKTYPE_DIRT <- 1
::BLOCKTYPE_STONE <- 2
::BLOCKTYPE_DIAMOND <- 3
::BLOCKTYPE_BEDROCK <- 4
::BLOCKTYPE_CHEST <- 5
::BLOCKTYPE_MOSSYSTONE <- 6

::TransparentBlocks <- {
	[BLOCKTYPE_CHEST] = true,
}

::OrientedBlocks <- {
	[BLOCKTYPE_CHEST] = true,
}

::Rewards <- {
	[BLOCKTYPE_GRASS] = 2,
	[BLOCKTYPE_DIRT] = 1,
	[BLOCKTYPE_STONE] = 2,
	[BLOCKTYPE_DIAMOND] = 50,
	[BLOCKTYPE_MOSSYSTONE] = 8,
}

::Blocks <- {}
::BlockCount <- 0
::AnnouncedCollapse <- false

::HintPrinter <- Entities.CreateByClassname("env_hudhint")

::CenterPrint <- function(ply, text)
{
	HintPrinter.__KeyValueFromString("message", text)
	EntFireByHandle(HintPrinter, "ShowHudHint", "", 0.0, ply, null)
}

::DebugPrint <- function(text)
{
	if (GetDeveloperLevel() > 0)
	{
		printl(text)
	}
}

::SeededRandom <- function(min, max)
{
   return floor(min + rand() / (RAND_MAX / (max - min + 1) + 1))
}

::RandTest <- function()
{
	for (local i = 0; i<100; i++)
	{
		printl(SeededRandom(1,10))
	}
}

::TeleportUp <- function(ply)
{
	if (ply.GetTeam() == 2)
	{
		ply.SetOrigin(Vector(-384, -32, 48))
	}
	else if (ply.GetTeam() == 3)
	{
		ply.SetOrigin(Vector(-384, 32, 48))
	}
	else
	{
		ply.SetOrigin(Vector(-384, 0, 48))
	}
}

::VecString <- function(vec)
{
	return floor(vec.x).tostring() + "_" + floor(vec.y).tostring() + "_" + floor(vec.z).tostring()
}

::BackToVec <- function(str)
{
	local c = split(str, "_")
	return Vector(c[0].tointeger(), c[1].tointeger(), c[2].tointeger())
}

::CollapseDigsite <- function()
{
	ScriptPrintMessageChatAll("The mine has collapsed!")

	ply <- null
	while ((ply = Entities.FindByClassname(ply, "*")) != null)
	{
		if (ply.GetClassname() == "player" || ply.GetClassname == "bot")
		{
			TeleportUp(ply)
		}
	}

	local garbage = []
	ent <- null
	while ((ent = Entities.FindByClassname(ent, "func_breakable")) != null)
	{
		garbage.push(ent)
		BlockCount--
	}
	// leftover chest lids
	ent <- null
	while ((ent = Entities.FindByClassname(ent, "func_door_rotating")) != null)
	{
		garbage.push(ent)
	}
	foreach (block in garbage)
	{
		block.Destroy()
	}
	DebugPrint("Got rid of " + garbage.len() + " edicts")

	Blocks = {}

	PlaceDefaultBlocks()
	AnnouncedCollapse = false

	SetupWorld()
}

::RenderAdjacentBlocks <- function(pos, ply_broke = true)
{
	local offsets = [
		Vector(BLOCK_WIDTH, 0, 0),
		Vector(-BLOCK_WIDTH, 0, 0),
		Vector(0, BLOCK_WIDTH, 0),
		Vector(0,-BLOCK_WIDTH,  0),
		Vector(0, 0, BLOCK_HEIGHT),
		Vector(0, 0, -BLOCK_HEIGHT),
	]

	foreach (offset in offsets)
	{
		local newpos = VecString(pos + offset)
		if (ply_broke)
		{
			foreach (num, dungeon in DungeonData)
			{
				if ("rendered" in dungeon && !dungeon.rendered && newpos in dungeon.blocks)
				{
					RenderDungeon(num)
				}
			}
		}
		if ((!(newpos in Blocks) || Blocks[newpos] == BLOCKSTATE_PENDING) && (pos + offset).z < (BLOCK_HEIGHT / 2))
		{
			CreateBlock(pos + offset)
		}
	}
}

::CheckForCollapse <- function()
{
	if (BlockCount > MAX_BLOCKS)
	{
		CollapseDigsite()
		return
	}
	else if (BlockCount > (MAX_BLOCKS * 0.9) && !AnnouncedCollapse)
	{
		AnnouncedCollapse = true
		ScriptPrintMessageChatAll("The mine is about to collapse!")
	}
}

::CreateBlock <- function(pos, type = -1)
{
	if (type == -1)
	{
		local blocklevel = (pos.z - (BLOCK_HEIGHT / 2)) / -BLOCK_HEIGHT
		if (blocklevel == 1)
		{
			type = BLOCKTYPE_GRASS
		}
		else if (blocklevel < 7)
		{
			type = BLOCKTYPE_DIRT
		}
		else if (blocklevel > 48)
		{
			type = BLOCKTYPE_BEDROCK
			
		}
		else if (blocklevel > 30 && SeededRandom(blocklevel, 60) == 60)
		{
			type = BLOCKTYPE_DIAMOND
		}
		else
		{
			type = BLOCKTYPE_STONE
		}
	}

	Blocks[VecString(pos)] <- BLOCKSTATE_OCCUPIED

	if (type in TransparentBlocks && TransparentBlocks[type])
	{
		RenderAdjacentBlocks(pos)
	}

	BlockMakers[type].SpawnEntityAtLocation(pos, Vector(0, 0, 0))
	BlockCount++
}

::ImportantPointsThreshold <- 20

::GivePoints <- function(ply, amt)
{
	if (amt > ImportantPointsThreshold)
	{
		CenterPrint(ply, "You got " + amt + " point" + (amt == 1 ? "" : "s") + "!")
	}
	if (ply.ValidateScriptScope())
	{
		local scope = ply.GetScriptScope()
		if ("points" in scope)
		{
			scope.points += amt
		}
		else
		{
			scope.points <- amt
		}
	}
}

::BlockBroken <- function(block, type = BLOCKTYPE_STONE)
{
	local pos = block.GetOrigin()
	Blocks[VecString(pos)] <- BLOCKSTATE_BROKEN

	RenderAdjacentBlocks(pos)

	EntFireByHandle(block, "Break", "", 0.0, null, null)
	SendToConsole("r_cleardecals")

	local breaker = Entities.FindByClassnameNearest("player", pos, 5000)
	if (breaker != null)
	{
		GivePoints(breaker, Rewards[type])
	}

	BlockCount--
}

::DungeonCount <- 0
::DungeonData <- {}

::BuildDungeonData <- function()
{
	DebugPrint("Building dungeon data...")

	for (local i = 0; i < SeededRandom(1, 3); i++)
	{
		DungeonData[i] <- {}

		DungeonData[i].width <- SeededRandom(3, 5)
		DungeonData[i].length <- SeededRandom(3, 5)
		DungeonData[i].height <- SeededRandom(2, 4)

		DungeonData[i].x <- SeededRandom(-30, 30)
		DungeonData[i].y <- SeededRandom(-30, 30)
		DungeonData[i].z <- SeededRandom(20, 35) * -1

		DungeonData[i].blocks <- {}
		DungeonData[i].blocks_floor <- {}
		DungeonData[i].blocks_chests <- {}

		local chests = 0
		for (local z = DungeonData[i].z - DungeonData[i].height; z <= DungeonData[i].z; z++)
		{
			for (local x = DungeonData[i].x; x <= DungeonData[i].x + DungeonData[i].width; x++)
			{
				for (local y = DungeonData[i].y; y <= DungeonData[i].y + DungeonData[i].length; y++)
				{	
					local vec = Vector((x * BLOCK_WIDTH) - (BLOCK_WIDTH / 2), (y * BLOCK_WIDTH) - (BLOCK_WIDTH / 2), (z * BLOCK_HEIGHT) - (BLOCK_HEIGHT / 2))
					local str_vec = VecString(vec)
					//printl("putting " + str_vec + " into dungeon " + i + "...")
					DungeonData[i].blocks[str_vec] <- true
					if (z == DungeonData[i].z - DungeonData[i].height)
					{
						DungeonData[i].blocks_floor[str_vec] <- true
					}
					else if (z == (DungeonData[i].z - DungeonData[i].height) + 1)
					{
						if (SeededRandom(1, 16 + (chests * chests)) == 1)
						{
							DungeonData[i].blocks_chests[str_vec] <- true
							chests++
						}
						// always spawn at least one chest
						if (chests == 0 && x == DungeonData[i].x + DungeonData[i].width && y == DungeonData[i].y + DungeonData[i].length)
						{
							DungeonData[i].blocks_chests[str_vec] <- true
						}
					}
				}
			}
		}

		DungeonData[i].rendered <- false
		DungeonCount++
	}

	DebugPrint("Built " + DungeonCount + " dungeons!")
}

::RevealDungeon <- function(n = 0)
{
	foreach (block, b in DungeonData[n].blocks)
	{
		DrawAxes(BackToVec(block), 64, 10, true)
		break
	}
}

::RevealAllDungeons <- function()
{
	for (local i = 0; i < DungeonCount; i++)
	{
		RevealDungeon(i)
	}
}

::RenderDungeon <- function(n = 0)
{
	if (DungeonData[n].rendered)
	{
		return
	}

	DungeonData[n].rendered <- true

	// doing bulk edict allocation here, which can mess things up REALLY badly
	// make sure there's plenty of room before doing this
	if (MAX_BLOCKS - BlockCount < 100)
	{
		CollapseDigsite()
		return
	}

	foreach (block, b in DungeonData[n].blocks)
	{
		Blocks[block] <- BLOCKSTATE_BROKEN
	}

	foreach (block, b in DungeonData[n].blocks)
	{
		RenderAdjacentBlocks(BackToVec(block), false)
	}

	foreach (block, b in DungeonData[n].blocks_floor)
	{
		CreateBlock(BackToVec(block), BLOCKTYPE_MOSSYSTONE)
	}

	foreach (block, b in DungeonData[n].blocks_chests)
	{
		CreateBlock(BackToVec(block), BLOCKTYPE_CHEST)
	}
}

::RenderAllDungeons <- function()
{
	for (local i = 0; i < DungeonCount; i++)
	{
		RenderDungeon(i)
	}
}

::PlaceDefaultBlocks <- function()
{
	local corner = Vector(((DIGSITE_WIDTH / 2) * -BLOCK_WIDTH) + (BLOCK_WIDTH / 2), ((DIGSITE_HEIGHT / 2) * -BLOCK_WIDTH) + (BLOCK_WIDTH / 2), BLOCK_HEIGHT / -2)
	for (local x = 0; x < DIGSITE_WIDTH; x++)
	{
		for (local y = 0; y < DIGSITE_HEIGHT; y++)
		{
			CreateBlock(corner + Vector(x * BLOCK_WIDTH, y * BLOCK_WIDTH, 0))
		}
	}
}

::DrawAxes <- function(pos, size = 16, time = 0.5, xray = false)
{
	DebugDrawLine(pos - Vector(size, 0, 0), pos + Vector(size, 0, 0), 255, 0, 0, xray, time)
	DebugDrawLine(pos - Vector(0, size, 0), pos + Vector(0, size, 0), 0, 255, 0, xray, time)
	DebugDrawLine(pos - Vector(0, 0, size), pos + Vector(0, 0, size), 0, 0, 255, xray, time)
}

::RefreshBlockCount <- function()
{
	local blocks = 0

	ent <- null
	while ((ent = Entities.FindByClassname(ent, "func_breakable")) != null)
	{
		blocks++
	}

	if (blocks != BlockCount)
	{
		DebugPrint("BlockCount discrepancy! Adjusting...")
		BlockCount <- blocks
	}
}

::EdictReport <- function()
{
	local total = 0
	local blocks = 0

	ent <- null
	while ((ent = Entities.FindByClassname(ent, "*")) != null)
	{
		total++
		if (ent.GetClassname() == "func_breakable")
		{
			blocks++
		}
	}

	printl("========== EDICT REPORT ==========")
	printl("Total edicts: " + total)
	printl("Total blocks: " + blocks)
	printl("Block count: " + BlockCount)
	printl("==================================")
}

::SetupWorld <- function()
{
	PlaceDefaultBlocks()
	BuildDungeonData()
}

OnPostSpawn <- function()
{
	SetupWorld()
}
