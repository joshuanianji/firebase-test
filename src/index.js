import * as firebase from "firebase/app";
import "firebase/auth";
import "firebase/firestore";

import { Elm } from "./Main.elm";
import registerServiceWorker from "./registerServiceWorker";

// Just checking envs are defined - Debug statement
if (process.env.ELM_APP_API_KEY !== undefined) {
    console.log(process.env.ELM_APP_API_KEY)
} else {
    console.log("false")
}

const firebaseConfig = {
    apiKey: process.env.ELM_APP_API_KEY,
    authDomain: process.env.ELM_APP_AUTH_DOMAIN,
    databaseURL: process.env.ELM_APP_DATABASE_URL,
    projectId: process.env.ELM_APP_PROJECT_ID,
    storageBucket: process.env.ELM_APP_STORAGE_BUCKET,
    messagingSenderId: process.env.ELM_APP_MESSAGING_SENDER_ID,
    appId: process.env.ELM_APP_APP_ID
};

firebase.initializeApp(firebaseConfig);

const provider = new firebase.auth.GoogleAuthProvider();
const db = firebase.firestore();

const app = Elm.Main.init({
    node: document.getElementById("root")
});

app.ports.signIn.subscribe(() => {
    console.log("LogIn called");
    firebase
        .auth()
        .signInWithPopup(provider)
        .then(result => {
            result.user.getIdToken().then(idToken => {
                console.log("login success!")
                console.log(result.user.uid)
                app.ports.signInInfo.send({
                    token: idToken,
                    email: result.user.email,
                    uid: result.user.uid
                });
            });
        })
        .catch(error => {
            console.log("sign in error!")
            app.ports.signInError.send({
                code: error.code,
                message: error.message
            });
        });
});

app.ports.signOut.subscribe(() => {
    console.log("LogOut called");
    firebase.auth().signOut();
});

//  Observer on user info
firebase.auth().onAuthStateChanged(user => {
    if (user) {
        user
            .getIdToken()
            .then(idToken => {
                app.ports.signInInfo.send({
                    token: idToken,
                    email: user.email,
                    uid: user.uid
                });
            })
            .catch(error => {
                console.log("Error when retrieving cached user");
                console.log(error);
            });

        // get all global messages
        update_global_chat()
    }
});

app.ports.saveMessage.subscribe(data => {
    console.log(`saving message to database : ${data.content}`);

    db.collection(`/chats/global/texts/`)
        .add(data)
        .then(() => {
            // callback - I think!
            console.log("Successfully stored text to cloud")
            update_global_chat()
        })
        .catch(error => {
            app.ports.signInError.send({
                code: error.code,
                message: error.message
            });
        }).catch(error => {
            console.log("Error when saving to database");
            console.log(error);
        });
});

registerServiceWorker();



// FUNCTIONS BELOW

// get global data and stuff

var update_global_chat = () =>
    db.collection(`/chats/global/texts/`)
        .orderBy("time") //newest get shown on bottom
        .get()
        .then(function (querySnapshot) {
            const messages = []

            querySnapshot.forEach(doc => {
                // doc.data() is never undefined for query doc snapshots
                if (doc.data().content) {
                    messages.push(doc.data());
                }
            });

            app.ports.receiveMessages.send({
                messages: messages
            });

            console.log("updated global chat!")
            console.log(messages.map(m => m.content))

        });