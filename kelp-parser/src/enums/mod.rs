use crate::create_dispatch_parser;
use minecraft_command_types::command::enums::bossbar_store_type::BossbarStoreType;
use minecraft_command_types::command::enums::difficulty::Difficulty;
use minecraft_command_types::command::enums::entity_anchor::EntityAnchor;
use minecraft_command_types::command::enums::gamemode::Gamemode;
use minecraft_command_types::command::enums::heightmap::Heightmap;
use minecraft_command_types::command::enums::if_blocks_mode::IfBlocksMode;
use minecraft_command_types::command::enums::relation::Relation;
use minecraft_command_types::command::enums::sort::Sort;
use minecraft_command_types::command::enums::store_type::StoreType;

create_dispatch_parser!(
    pub fn parse_gamemode -> Gamemode,
    "gamemode",
    Gamemode::Survival,
    identifier,
    {
        ("adventure", "a") => Gamemode::Adventure,
        ("survival", "s") => Gamemode::Survival,
        ("creative", "c") => Gamemode::Creative,
        ("spectator", "sp") => Gamemode::Spectator,
    }
);

create_dispatch_parser!(
    pub fn parse_sort -> Sort,
    "sort",
    Sort::Arbitrary,
    identifier,
    {
        ("arbitrary") => Sort::Arbitrary,
        ("furthest") => Sort::Furthest,
        ("nearest") => Sort::Nearest,
        ("random") => Sort::Random,
    }
);

create_dispatch_parser!(
    pub fn parse_difficulty -> Difficulty,
    "difficulty",
    Difficulty::Easy,
    identifier,
    {
        ("easy") => Difficulty::Easy,
        ("normal") => Difficulty::Normal,
        ("hard") => Difficulty::Hard,
        ("peaceful") => Difficulty::Peaceful,
    }
);

create_dispatch_parser!(
    pub fn parse_if_blocks_mode -> IfBlocksMode,
    "if_blocks_mode",
    IfBlocksMode::All,
    identifier,
    {
        ("all") => IfBlocksMode::All,
        ("masked") => IfBlocksMode::Masked,
    }
);

create_dispatch_parser!(
    pub fn parse_bossbar_store_type -> BossbarStoreType,
    "bossbar_store_type",
    BossbarStoreType::Value,
    identifier,
    {
        ("value") => BossbarStoreType::Value,
        ("max") => BossbarStoreType::Max,
    }
);

create_dispatch_parser!(
    pub fn parse_store_type -> StoreType,
    "store_type",
    StoreType::Result,
    identifier,
    {
        ("result") => StoreType::Result,
        ("success") => StoreType::Success,
    }
);

create_dispatch_parser!(
    pub fn parse_entity_anchor -> EntityAnchor,
    "entity_anchor",
    EntityAnchor::Eyes,
    identifier,
    {
        ("eyes") => EntityAnchor::Eyes,
        ("feet") => EntityAnchor::Feet,
    }
);

create_dispatch_parser!(
    pub fn parse_relation -> Relation,
    "relation",
    Relation::Attacker,
    identifier,
    {
        ("attacker") => Relation::Attacker,
        ("controller") => Relation::Controller,
        ("leasher") => Relation::Leasher,
        ("origin") => Relation::Origin,
        ("owner") => Relation::Owner,
        ("passengers") => Relation::Passengers,
        ("target") => Relation::Target,
        ("vehicle") => Relation::Vehicle,
    }
);

create_dispatch_parser!(
    pub fn parse_heightmap -> Heightmap,
    "heightmap",
    Heightmap::WorldSurface,
    identifier,
    {
        ("world_surface") => Heightmap::WorldSurface,
        ("motion_blocking_no_leaves") => Heightmap::MotionBlockingNoLeaves,
        ("motion_blocking") => Heightmap::MotionBlocking,
        ("ocean_floor") => Heightmap::OceanFloor,
    }
);
