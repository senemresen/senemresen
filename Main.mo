import Array "mo:base/Array";
import Hash "mo:base/Hash";
import HashMap "mo:base/HashMap";
import Iter "mo:base/Iter";
import Nat "mo:base/Nat";
import Text "mo:base/Text";
import Int "mo:base/Int";
import Time "mo:base/Time";

actor class HealthCoach() {
    public type UserProfile = {
        name : Text;
        age : Nat;
        weight : Nat;
        height : Nat;
        goal : Text;
    };

    public type Activity = {
        date : Int;
        meals : [Text];
        sleep : Nat;
        exercise : Nat;
        water : Nat;
    };

    let users = HashMap.HashMap<Text, UserProfile>(1, Text.equal, Text.hash);
    let activities = HashMap.HashMap<Text, [Activity]>(1, Text.equal, Text.hash);

    public shared(msg) func addUser(
        id: Text, 
        name: Text, 
        age: Nat, 
        weight: Nat,
        height: Nat,
        goal: Text
    ) : async Bool {
        let profile : UserProfile = {
            name = name;
            age = age;
            weight = weight;
            height = height;
            goal = goal;
        };
        users.put(id, profile);
        activities.put(id, []);
        return true;
    };

    public shared(msg) func addActivity(
        id: Text,
        meals: [Text],
        sleep: Nat,
        exercise: Nat,
        water: Nat
    ) : async Bool {
        let activity : Activity = {
            date = Time.now();
            meals = meals;
            sleep = sleep;
            exercise = exercise;
            water = water;
        };

        switch (activities.get(id)) {
            case null { return false };
            case (?userActivities) {
                let newActivities = Array.append<Activity>(userActivities, [activity]);
                activities.put(id, newActivities);
                return true;
            };
        };
    };

    public query func getRecommendations(id: Text) : async Text {
        switch (users.get(id)) {
            case null { return "Kullanıcı bulunamadı" };
            case (?user) {
                var response = "Öneriler:\n";
                
                if (user.goal == "weight_loss") {
                    response #= "\nKilo verme önerileri:\n";
                    response #= "- Günlük kalori açığı oluşturun\n";
                    response #= "- Kardiyo egzersizleri yapın\n";
                    response #= "- Protein ağırlıklı beslenin\n";
                } else if (user.goal == "muscle_gain") {
                    response #= "\nKas kazanma önerileri:\n";
                    response #= "- Protein alımını artırın\n";
                    response #= "- Ağırlık antrenmanı yapın\n";
                    response #= "- Kalori fazlası oluşturun\n";
                } else {
                    response #= "\nGenel öneriler:\n";
                    response #= "- Dengeli beslenin\n";
                    response #= "- Düzenli egzersiz yapın\n";
                    response #= "- Bol su için\n";
                };

                switch (activities.get(id)) {
                    case null { return response };
                    case (?acts) {
                        if (acts.size() > 0) {
                            let last = acts[acts.size() - 1];
                            if (last.sleep < 7) {
                                response #= "\nUyku önerileri:\n";
                                response #= "- Daha fazla uyuyun\n";
                                response #= "- Uyku kalitenizi artırın\n";
                            };
                            if (last.water < 2000) {
                                response #= "\nSu tüketimi önerileri:\n";
                                response #= "- Daha fazla su için\n";
                                response #= "- Su şişesi taşıyın\n";
                            };
                        };
                    };
                };
                return response;
            };
        };
    };

    public query func getProgress(id: Text) : async Text {
        switch (activities.get(id)) {
            case null { return "Kullanıcı bulunamadı" };
            case (?acts) {
                if (acts.size() == 0) {
                    return "Henüz aktivite kaydı yok";
                };

                var response = "İlerleme Raporu:\n";
                let size = acts.size();
                let start = if (size > 7) size - 7 else 0;
                let recent = Array.subArray(acts, start, size - start);
                
                var totalSleep = 0;
                var totalExercise = 0;
                var totalWater = 0;
                
                for (act in recent.vals()) {
                    totalSleep += act.sleep;
                    totalExercise += act.exercise;
                    totalWater += act.water;
                };

                let days = recent.size();
                response #= "\nSon " # Int.toText(days) # " günlük ortalamalar:";
                response #= "\nUyku: " # Int.toText(totalSleep / days) # " saat";
                response #= "\nEgzersiz: " # Int.toText(totalExercise / days) # " dakika";
                response #= "\nSu: " # Int.toText(totalWater / days) # " ml";
                
                return response;
            };
        };
    };
}
