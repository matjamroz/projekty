select * from world_cup_matches wcm;
select * from world_cups wc;


update  world_cup_matches
set Win_Conditions = 'Regular time'
WHERE Win_Conditions IS NULL OR Win_Conditions = '';

update  world_cup_matches
set Win_Conditions = 'Penalties'
WHERE Win_Conditions like '%penalties%';

# Która drużyna wygrała najwięcej meczów, ale nigdy nie zdobyła tytułu mistrza świata?
with Wins as  (
    select Home_Team AS Team, COUNT(*) AS Wins
    from  world_cup_matches
    where Home_Goals > Away_Goals
    group  by Home_Team
    union  all 
    select Away_Team AS Team, COUNT(*) AS Wins
    from  world_cup_matches
    where Away_Goals > Home_Goals
    group by Away_Team
)
select w.Team, sum(w.Wins) AS Total_Wins
from Wins w
left join  world_cups wc on  w.Team = wc.Winner
where wc.Winner is  null 
group by  w.Team
order by Total_Wins desc



# Jakie kraje najczęściej wygrywały w dogrywce na Mistrzostwach Świata?
with ExtraTimeMatches as (
    select Home_Team as Team, Home_Goals, Away_Goals, Win_Conditions
    from world_cup_matches
    where Win_Conditions = 'Extra time' or Win_Conditions = 'Golden goal'
    union all
    select Away_Team as Team, Away_Goals,Home_Goals,  Win_Conditions
    from world_cup_matches
    where Win_Conditions = 'Extra time' or Win_Conditions = 'Golden goal'
),
WinsInExtraTime as (
    select Team, count(*) as Wins
    from ExtraTimeMatches
    where Home_Goals > Away_Goals
    group by Team
)
select Team, Wins
from WinsInExtraTime
order by ] Wins desc;



# Które drużyny miały najlepszą różnicę bramek w fazach grupowych w każdym turnieju
with groupstagestats as (
    select
        m.year,
        m.stage,
        m.Home_Team as team,
        m.Home_Goals as goals_scored,
        m.Away_Goals as goals_conceded
    from world_cup_matches m
    where m.stage = 'Group Stage' or m.stage = 'First group stage' or m.stage = 'Second group stage'
        and m.Home_Goals is not null 
        and m.Away_Goals is not null
    union all
    select
        m.year,
        m.stage,
        m.Away_Team as team,
        m.Away_Goals as goals_scored,
        m.Home_Goals as goals_conceded
    from world_cup_matches m
    where m.stage = 'Group Stage' or m.stage = 'First group stage' or m.stage = 'Second group stage'
        and m.Home_Goals is not null 
        and m.Away_Goals is not null
),
goaldifference as (
    select
        year,
        team,
        sum(goals_scored) as goals_scored,
        sum(goals_conceded) as goals_conceded,
        sum(goals_scored - goals_conceded) as goal_difference,
        count(*) as matches_played
    from groupstagestats
    group by year, team
)
select *
from goaldifference gd
order by year desc, goal_difference desc;






