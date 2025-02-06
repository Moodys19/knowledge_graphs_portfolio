# Knowledge Graph Portfolio
For this project I will be using the EA Sports FC 24 complete player dataset from Kaggle.
I will not be able to implement the original proposal due to issues in finding useful data within the time constraints 
I am facing. Thus I will be adapting my approach: Wir schauen uns das [EA Sports FC 24 complete player dataset](https://www.kaggle.com/datasets/stefanoleone992/ea-sports-fc-24-complete-player-dataset/?select=male_teams.csv). Die idee ist das wir das ganze bauen und wenn das für diesen Datensatz funktioniert dann sollte das auch für echte Daten funktionieren.


Wir predicten potential, hier ist das ein regression task, im real life könnte man hier klar ein Classfication Task draus machen.


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


TODO:
    - yaml am schluss updaten
    - comments managen


das ganze kann man das mit den Verletzungen als data properties auch machen wenn alles klappt


Lernziele:
    - financial: fußball ist ein milionengeschäft - insights könnten sehr relvante für clubs sein yada yada
    - Embeddings - erkläre was das ist und dass wir das verwenden könnten um Vereine und Spieler zu clustern 


Challenges:
    - Daten größe -> KG zu groß zum runnen der Modelle
    - Train/Test split muss sofisticated sein

No large gap between training and validation loss → No strong sign of overfitting und daher kein Dropout!.


issues with the dataset:
    club_position hat das Problem das wenn Spieler verletzt sind oder sonst was 
    #' die nicht in der Aufstellung sind obwohl sie eigentlisch Stammspieler sind (De Bruyne z.B.) 
    #' irl müsste man ein anderes Scoring verwenden -> z.B.: ratio starting XI (oder > 30 min played) zu games available
