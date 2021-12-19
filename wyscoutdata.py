import numpy as np
import pandas as pd
import xgboost
from io import BytesIO
from pathlib import Path
from urllib.parse import urlparse
from urllib.request import urlopen, urlretrieve
from zipfile import ZipFile, is_zipfile

import os
import warnings
from tqdm import tqdm

warnings.simplefilter(action="ignore", category=pd.errors.PerformanceWarning)

import spadl as spadl
import spadl.wyscout as wyscout

import vaep.features as fs
import vaep.labels as lab
import vaep.formula as vaepformula

import ssl

ssl._create_default_https_context = ssl._create_unverified_context


def read_json_file(filename):
    with open(filename, "rb") as json_file:
        return BytesIO(json_file.read()).getvalue().decode("unicode_escape")





class WyScoutData:
    def __init__(self):
        self.competitions = None
        self.games_dict = dict()
        self.events = dict()
        self.actions = dict()
        self.compt_games = dict()
        self.loader = wyscout.WyscoutLoader()
        self._data_dir = """Replace this with the path where the data should be stored"""
        self.labels = dict()
        self.features = dict()
        self.models = dict()
        self.pred = dict()
        self.actiontypes = spadl.actiontypes_df()
        self.bodyparts = spadl.bodyparts_df()
        self.results = spadl.results_df()
        self.vaep_scores = dict()
        self.mvp = None

    def download_wyscout_data(self):
        dataset_urls = dict(
            competitions='https://ndownloader.figshare.com/files/15073685',
            teams='https://ndownloader.figshare.com/files/15073697',
            players='https://ndownloader.figshare.com/files/15073721',
            games='https://ndownloader.figshare.com/files/14464622',
            events='https://ndownloader.figshare.com/files/14464685',
        )

        raw_datafolder = os.path.join(self._data_dir)
        if not os.path.exists(raw_datafolder):
            os.makedirs(raw_datafolder, exist_ok=True)

        # download and unzip Wyscout open data
        for url in tqdm(dataset_urls.values(), desc='Downloading data'):
            url_obj = urlopen(url).geturl()
            path = Path(urlparse(url_obj).path)
            file_name = os.path.join(raw_datafolder, path.name)
            file_local, _ = urlretrieve(url_obj, file_name)
            if is_zipfile(file_local):
                with ZipFile(file_local) as zip_file:
                    zip_file.extractall(raw_datafolder)

    def convert_wyscout_data(self):
        raw_data = os.path.join(self._data_dir)

        json_competiton = read_json_file(f"{raw_data}/competitions.json")
        df_competitions = pd.read_json(json_competiton)
        competitions = wyscout.convert_competitions(df_competitions)
        self.competitions = competitions
        self.competitions["season_id"] = [181248, 181150, 181144, 181189, 181137, 9291, 10078]
        self.competitions["season_name"] = ['2017/2018', '2017/2018', '2017/2018', '2017/2018', '2017/2018',
                                            '2016', '2018']

        json_teams = read_json_file(f"{raw_data}/teams.json")
        df_teams = wyscout.convert_teams(pd.read_json(json_teams))
        self.teams_dict = dict()
        for key, value in zip(df_teams.team_id, df_teams.team_name):
            self.teams_dict[key] = value

        json_players = read_json_file(f"{raw_data}/players.json")
        df_players = wyscout.convert_players(pd.read_json(json_players))
        self.player_dict = dict()
        for key, value in zip(df_players.player_id, df_players.player_name):
            self.player_dict[key] = value
        name = None
        for competition in df_competitions.itertuples():
            if competition.country_name != "International":
                json_games = read_json_file(
                    f"{raw_data}/matches_{competition.country_name.replace(' ', '_')}.json"
                )
                name = competition.country_name.replace(' ', '_')
            else:
                json_games = read_json_file(
                    f"{raw_data}/matches_{competition.name.replace(' ', '_')}.json"
                )
                name = competition.name.replace(' ', '_')

            df_games = pd.read_json(json_games)
            df_games = wyscout.convert_games(df_games)
            df_games["home_team"] = df_games.home_team_id.map(self.teams_dict)
            df_games["away_team"] = df_games.away_team_id.map(self.teams_dict)

            self.compt_games[name] = df_games

            json_events = read_json_file(
                f"{raw_data}/events_{name}.json"
            )
            df_events = pd.read_json(json_events).groupby("matchId", as_index=False)


            for game in tqdm(list(df_games.itertuples())):
                game_id = game.game_id
                game_events = wyscout.convert_events(df_events.get_group(game_id))
                self.games_dict[game_id] = game_events
                home_team = game.home_team_id
                df_actions = wyscout.convert_to_actions(game_events, home_team)



                df_actions["action_id"] = range(len(df_actions))

                self.actions[game_id] = df_actions

    def store_data(self):
        print("Storing competitions...")
        path = self._data_dir
        self.competitions.to_json(f"{path}/competitions.json", orient="index")

        for comp in self.compt_games.keys():
            print("Storing matches...")
            path_match = f"/Users/sinan/Documents/DFGSProjekt/wyscoutdatatest/matches_{comp}.json"
            self.compt_games[comp].to_json(path_match, orient="index")

        datafolder = f"{path}/actions"
        if not os.path.exists(datafolder):
            os.mkdir(datafolder)
            print(f"Directory {datafolder} created")

        for game_id in self.actions.keys():
            path = f"{datafolder}/actions_{game_id}.json"
            action = self.actions[game_id]
            action["team_name"] = action.team_id.map(self.teams_dict)

            action["player_id"] = action.player_id.replace(self.player_dict)

            action.rename(columns={"player_id": "player_name"}, inplace=True)

            action = (
                action.merge(self.actiontypes, how="left")
                .merge(self.bodyparts, how="left")
                .merge(self.results, how="left")
                .reset_index(drop=True)
            )

            cols = ["bodypart_id", "type_id", "result_id"]
            action.drop(cols, axis=1, inplace=True)
            action = action[["game_id", "period_id", "time_seconds", "team_id", "team_name", "player_name", "start_x",
                             "start_y", "end_x", "end_y", "original_event_id", "action_id", "type_name", "bodypart_name",
                             "result_name"]]

            action.to_json(path, orient="index")


    def store_vaep(self):
        folder = f"{self._data_dir}/vaep_scores/"
        if not os.path.exists(folder):
            os.mkdir(folder)
            print(f"Directory {folder} created")

        for i in self.vaep_scores.keys():
            path = f"{folder}vaep_score_{i}.json"
            self.vaep_scores[i].to_json(path, orient="index")

        if self.mvp is not None:
            mvp = f"{path}/mvp"
            if not os.path.exists(mvp):
                os.mkdir(mvp)
                print(f"Directory {mvp} created")

            for i in self.mvp.keys():
                path = f"{mvp}/mvp_game_{i}.json"
                self.mvp[i].to_json(path, orient="index")
        else:
            pass


    def compute_features(self):
        xfns = [#fs.actiontype,
                fs.actiontype_onehot,
                #fs.bodypart,
                fs.bodypart_onehot,
                #fs.result,
                fs.result_onehot,
                fs.goalscore,
                fs.startlocation,
                fs.endlocation,
                fs.movement,
                fs.space_delta,
                fs.startpolar,
                fs.endpolar,
                fs.team,
                #fs.time,
                fs.time_delta]

        for competition in self.compt_games.keys():
            games = self.compt_games[competition]
            for game in tqdm(list(games.itertuples()), desc="genereating and storing features..."):
                actions = self.actions[game.game_id]
                actions = (
                    actions.merge(self.actiontypes, how="left")
                        .merge(self.results, how="left")
                        .merge(self.bodyparts, how="left")
                        .reset_index(drop=True)
                )
                gamestates = fs.gamestates(actions, 3)
                gamestates = fs.play_left_to_right(gamestates, game.home_team_id)


                X = pd.concat([fn(gamestates) for fn in xfns], axis=1)
                self.features[game.game_id] = X

    def compute_labels(self):
        yfns = [lab.scores, lab.concedes, lab.goal_from_shot]

        for i in self.compt_games.keys():
            games = self.compt_games[i]
            for game in tqdm(list(games.itertuples()), desc="computing and storing labels"):
                actions = self.actions[game.game_id]
                actions = (
                    actions.merge(self.actiontypes, how="left")
                        .merge(self.results, how="left")
                        .merge(self.bodyparts, how="left")
                        .reset_index(drop=True)
                )
                Y = pd.concat([fn(actions) for fn in yfns], axis=1)
                self.labels[game.game_id] = Y

    def getXY(self, games, Xcols):
        X = []
        for game_id in tqdm(games.game_id, desc="Selecting features"):
            Xi = self.features[game_id]
            X.append(Xi[Xcols])
        X = pd.concat(X).reset_index(drop=True)

        Ycols = ["scores", "concedes"]
        Y = []
        for game_id in tqdm(games.game_id, desc="Selecting label"):
            Yi = self.labels[game_id]
            Y.append(Yi[Ycols])
        Y = pd.concat(Y).reset_index(drop=True)

        return X, Y


    def preprocessing(self):
        self.all_games = []
        for game_id in self.actions.keys():
            self.all_games.append(self.actions[game_id])
        self.all_games = pd.concat(self.all_games).reset_index(drop=True)

        print("nb of games:", len(self.all_games))

        xfns = [#fs.actiontype,
                fs.actiontype_onehot,
                fs.bodypart_onehot,
                #fs.result,
                fs.result_onehot,
                fs.goalscore,
                fs.startlocation,
                fs.endlocation,
                fs.movement,
                fs.space_delta,
                fs.startpolar,
                fs.endpolar,
                #fs.team,
                fs.time_delta]

        nb_prev_actions = 3
        XCols = fs.feature_column_names(xfns, nb_prev_actions)
        self.X = list()
        for i in tqdm(self.features.keys(), total=len(self.features.keys()), desc="selecting features.."):
            self.X.append(self.features[i][XCols])
        self.X = pd.concat(self.X).reset_index(drop=True)
        collabs = ["scores", "concedes"]
        self.Y = list()
        for i in tqdm(self.labels.keys(), total=len(self.labels.keys()), desc="selecting labels.."):
            self.Y.append(self.labels[i][collabs])
        self.Y = pd.concat(self.Y).reset_index(drop=True)

        self.X = self.X.fillna(0)

    def train(self):
        for col in list(self.Y.columns):
            model = xgboost.XGBClassifier(n_estimators=50, max_depth=3, n_jobs=-3, verbosity=2)
            model.fit(self.X, self.Y[col])
            self.models[col] = model

    def predict(self):
        testX, testY = self.X, self.Y
        self.Y_hat = pd.DataFrame()
        for col in testY.columns:
            self.Y_hat[col] = [p[1] for p in self.models[col].predict_proba(testX)]

        A = []
        for game_id in tqdm(self.actions.keys(), total=len(self.actions.keys()), desc="Loading game ids"):
            A.append(self.actions[game_id]["game_id"])
        A = pd.concat(A).reset_index(drop=True)
        print("grouping predictions...")
        grouped_predictions = pd.concat([A, self.Y_hat], axis=1).groupby("game_id")
        for k, df in tqdm(grouped_predictions, desc="storing predictions"):
            df = df.reset_index(drop=True)
            self.pred[k] = df[self.Y_hat.columns]



    def vaep(self):
        #A = []
        for i in self.compt_games.keys():
            games = self.compt_games[i]
            for game in tqdm(list(games.itertuples()), desc="rating actions"):
                actions = self.actions[game.game_id]
                actions = (
                    actions
                        .merge(self.actiontypes, how="left")
                        .merge(self.bodyparts, how="left")
                        .merge(self.results, how="left")
                        .sort_values(["game_id", "period_id", "action_id"])
                        .reset_index(drop=True)
                )
                pred = self.pred[game.game_id]
                values = vaepformula.value(actions, pred.scores, pred.concedes)
                self.vaep_scores[game.game_id] = pd.concat([actions, pred, values], axis=1).sort_values(["game_id", "period_id", "time_seconds"]).reset_index(drop=True)

    def most_valuables_player(self):
        for game_id in self.vaep_scores.keys():
            game = self.vaep_scores[game_id]
            game["count"] = 1
            playersR = (
                game[["player_id", "vaep_value", "offensive_value", "defensive_value", "count"]]
                .groupby(["player_id"])
                .sum()
                .reset_index()
            )
            playersR["player_id"] = game.player_name.map(self.player_dict)
            playersR = playersR[["player_id", "player_name", "vaep_value", "offensive_value", "defensive_value", "count"]]

            self.mvp[game_id] = playersR

    def calculate_vaep_scores(self):
        self.download_wyscout_data()
        print("converting")
        self.convert_wyscout_data()
        print("features")
        self.compute_features()
        print("labels")
        self.compute_labels()
        print("preprocessing")
        self.preprocessing()
        print("train")
        self.train()
        print("predict")
        self.predict()
        self.vaep()
        self.store_data()
        self.store_vaep()


if __name__ == "__main__":
    wy = WyScoutData()
    wy.calculate_vaep_scores()

