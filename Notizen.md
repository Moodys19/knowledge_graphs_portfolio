# Knowledge Graph Portfolio
For this project I will be using the EA Sports FC 24 complete player dataset from Kaggle.
I will not be able to implement the original proposal due to issues in finding useful data within the time constraints 
I am facing. Thus I will be adapting my approach: Wir schauen uns das [EA Sports FC 24 complete player dataset](https://www.kaggle.com/datasets/stefanoleone992/ea-sports-fc-24-complete-player-dataset/?select=male_teams.csv). Die idee ist das wir das ganze bauen und wenn das für diesen Datensatz funktioniert dann sollte das auch für echte Daten funktionieren.

## KG Generation
Nodes:

    Players (player_id):
        Attributes: Skills (overall, potential, etc.), demographics (age, nationality_name), financials (value_eur, wage_eur), etc.
    Teams (club_team_id):
        Attributes: Team-level features (aggregates like average overall, total value_eur).
    Leagues (league_id):
        Attributes:
            Average team overall and value_eur within the league.
            Number of teams or players in the league.

Edges:

    Player-Belongs-To-Team:
        Connect players to teams (player_id → club_team_id).
    Player-Plays-With-Player:
        Connect players within the same team (player_id ↔ player_id).
    Team-Plays-In-League:
        Connect teams to leagues (club_team_id → league_id).
    Optional:
        League-Has-Player: A direct connection between players and leagues, if needed for analysis.