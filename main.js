var player;

function onYouTubeIframeAPIReady() {
    player = new YT.Player('player', {
        height: '390',
        width: '640',
        videoId: 'M7lc1UVf-VE',
        playerVars: {
            'playsinline': 1
        },
        events: {
            'onReady': onPlayerReady,
            'onStateChange': onPlayerStateChange
        }
    });
}

// 4. The API will call this function when the video player is ready.
function onPlayerReady(event) {
    event.target.playVideo();
}

// 5. The API calls this function when the player's state changes.
//    The function indicates that when playing a video (state=1),
//    the player should play for six seconds and then stop.
var done = false;
function onPlayerStateChange(event) {
    if (event.data == YT.PlayerState.PLAYING && !done) {
        setTimeout(stopVideo, 6000);
        done = true;
    }
}
function stopVideo() {
    player.stopVideo();
}


function parseHash () {
    if (!document.location.hash || document.location.hash == '#')
        return [];
    const res = document.location.hash.substr(1).split(',').map(
        (video, ix) => [ix, ...video.split('.')]
    );
    if (res.every(el => el.length == 3))
        return res;
    else
        throw "Invalid format!";
}

const model = { players: {}, count: 0 };

const players = model.players;

function createPlayer(ix, videoId, volume) {
    const divId = videoId;
    const div = document.createElement('div');
    div.id = ix.toString();
    document.querySelector('#players').appendChild(div);

    const player = new YT.Player(div, {
        height: '390',
        width: '640',
        videoId: videoId,
        playerVars: {
            'playsinline': 1
        },
        enablejsapi: 1,
        autoplay: 1,
        loop: 1,
        events: {
            'onReady': event => {
                event.target.playVideo();
                player.setVolume(volume);
                player.setLoop(true);
            }
        }
    });
    players[ix] = player;
}

function putPlayers (videos) {
    videos.forEach(([ix, videoId, volume]) => {
        if (players[ix]) {
            // TODO: exact check?
            if (players[ix].getVideoUrl().includes(videoId)) {
                // no-op
            } else {
                players[ix].loadVideoById(videoId);
            }
        } else {
            createPlayer(ix, videoId, volume);
        }
    });
}

const addField = (ix, videoId, volume) => {
    const linkInput = document.createElement('input');
    linkInput.type = 'text';
    linkInput.style = 'width: 500px;';
    linkInput.className = 'link';
    linkInput.value = 'https://www.youtube.com/watch?v=' + videoId;
    linkInput.addEventListener('change', () => {
        applyChanges();
    });

    const volumeInput = document.createElement('input');
    volumeInput.type = 'range';
    volumeInput.className = 'volume';
    volumeInput.min = 0;
    volumeInput.max = 100;
    volumeInput.step = 1;
    volumeInput.value = volume;
    volumeInput.addEventListener('change', () => {
        players[ix].setVolume(volumeInput.value);
        applyChanges();
    });
    volumeInput.addEventListener('mousemove', () => players[ix].setVolume(volumeInput.value));

    const delInput = document.createElement('span');
    delInput.className = "button-small del-button";
    delInput.textContent = "X";

    const row = document.createElement('div');
    row.appendChild(linkInput);
    row.appendChild(volumeInput);
    row.appendChild(delInput);
    row.className = 'entry';
    row.id = ix;
    delInput.addEventListener('click', _ => {
        row.remove();
        players[ix].destroy();
        players[ix] = undefined;
        document.getElementById(ix.toString()).remove();
        applyChanges();
    });
    document.querySelector('#fields').appendChild(row);
};

function putFields (videos) {
    videos.forEach(([ix, videoId, volume]) => {
        addField(ix, videoId, volume);
    });
}

const parseId = link =>
      link.split('=')[1] || '';

function getDataFromFields() {
    const entries = document.querySelector('#fields').getElementsByClassName('entry');
    const res = [];
    for (let entry of entries) {
        const linkEl = entry.getElementsByClassName('link')[0];
        const link = linkEl.value;
        if (parseId(link).length == 0) {
            linkEl.style.borderColor = 'red';
            throw "Invalid link";
        } else {
            linkEl.style.borderColor = 'unset';
        }
        const volume = entry.getElementsByClassName('volume')[0].value;
        res.push([parseInt(entry.id), parseId(link), parseInt(volume)]);
    }
    return res;
};

function getHashForVideos (videos) {
    return videos.map(([_, videoId, volume]) => videoId + '.' + volume).join(',');
}

function applyChanges () {
    const videos = getDataFromFields();
    const link = document.location.protocol + '//' + document.location.host + '#' + getHashForVideos(videos);
    putPlayers(videos);
    document.location.replace(link);
    document.getElementById('mix-link').href = link;
    const updatedHandle = document.getElementById('updated-handle');
    updatedHandle.style.display = 'inline';
    setTimeout(() => updatedHandle.style.display = 'none', 1000);

}

function setHandlers () {
    document.querySelector('#add-field').addEventListener('click', () => {
        addField(model.count, '', 100);
        model.count++;
    });
}

function init () {
    const videos = parseHash();
    model.count = videos.length;
    putPlayers(videos);
    putFields(videos);
    setHandlers();
    document.getElementById('mix-link').href = document.location.href;
}

// 2. This code loads the IFrame Player API code asynchronously.
var tag = document.createElement('script');

tag.src = "https://www.youtube.com/iframe_api";
var firstScriptTag = document.getElementsByTagName('script')[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

function onYouTubeIframeAPIReady() {
    console.log('init');
    init();
}
