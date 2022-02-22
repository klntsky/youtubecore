var player;

// From can-autoplay.js

const AUDIO = new Blob([new Uint8Array([255, 227, 24, 196, 0, 0, 0, 3, 72, 1, 64, 0, 0, 4, 132, 16, 31, 227, 192, 225, 76, 255, 67, 12, 255, 221, 27, 255, 228, 97, 73, 63, 255, 195, 131, 69, 192, 232, 223, 255, 255, 207, 102, 239, 255, 255, 255, 101, 158, 206, 70, 20, 59, 255, 254, 95, 70, 149, 66, 4, 16, 128, 0, 2, 2, 32, 240, 138, 255, 36, 106, 183, 255, 227, 24, 196, 59, 11, 34, 62, 80, 49, 135, 40, 0, 253, 29, 191, 209, 200, 141, 71, 7, 255, 252, 152, 74, 15, 130, 33, 185, 6, 63, 255, 252, 195, 70, 203, 86, 53, 15, 255, 255, 247, 103, 76, 121, 64, 32, 47, 255, 34, 227, 194, 209, 138, 76, 65, 77, 69, 51, 46, 57, 55, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 255, 227, 24, 196, 73, 13, 153, 210, 100, 81, 135, 56, 0, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170, 170])], {type: 'audio/mpeg'});

/**
 * @type {Blob}
 */
const VIDEO = new Blob([new Uint8Array([0, 0, 0, 28, 102, 116, 121, 112, 105, 115, 111, 109, 0, 0, 2, 0, 105, 115, 111, 109, 105, 115, 111, 50, 109, 112, 52, 49, 0, 0, 0, 8, 102, 114, 101, 101, 0, 0, 2, 239, 109, 100, 97, 116, 33, 16, 5, 32, 164, 27, 255, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55, 167, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 112, 33, 16, 5, 32, 164, 27, 255, 192, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 55, 167, 128, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 112, 0, 0, 2, 194, 109, 111, 111, 118, 0, 0, 0, 108, 109, 118, 104, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 232, 0, 0, 0, 47, 0, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, 0, 1, 236, 116, 114, 97, 107, 0, 0, 0, 92, 116, 107, 104, 100, 0, 0, 0, 3, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 0, 0, 0, 0, 0, 47, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 64, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 101, 100, 116, 115, 0, 0, 0, 28, 101, 108, 115, 116, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 47, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 100, 109, 100, 105, 97, 0, 0, 0, 32, 109, 100, 104, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 172, 68, 0, 0, 8, 0, 85, 196, 0, 0, 0, 0, 0, 45, 104, 100, 108, 114, 0, 0, 0, 0, 0, 0, 0, 0, 115, 111, 117, 110, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 83, 111, 117, 110, 100, 72, 97, 110, 100, 108, 101, 114, 0, 0, 0, 1, 15, 109, 105, 110, 102, 0, 0, 0, 16, 115, 109, 104, 100, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 36, 100, 105, 110, 102, 0, 0, 0, 28, 100, 114, 101, 102, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 12, 117, 114, 108, 32, 0, 0, 0, 1, 0, 0, 0, 211, 115, 116, 98, 108, 0, 0, 0, 103, 115, 116, 115, 100, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 87, 109, 112, 52, 97, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 16, 0, 0, 0, 0, 172, 68, 0, 0, 0, 0, 0, 51, 101, 115, 100, 115, 0, 0, 0, 0, 3, 128, 128, 128, 34, 0, 2, 0, 4, 128, 128, 128, 20, 64, 21, 0, 0, 0, 0, 1, 244, 0, 0, 1, 243, 249, 5, 128, 128, 128, 2, 18, 16, 6, 128, 128, 128, 1, 2, 0, 0, 0, 24, 115, 116, 116, 115, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 4, 0, 0, 0, 0, 28, 115, 116, 115, 99, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 1, 0, 0, 0, 2, 0, 0, 0, 1, 0, 0, 0, 28, 115, 116, 115, 122, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2, 0, 0, 1, 115, 0, 0, 1, 116, 0, 0, 0, 20, 115, 116, 99, 111, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 44, 0, 0, 0, 98, 117, 100, 116, 97, 0, 0, 0, 90, 109, 101, 116, 97, 0, 0, 0, 0, 0, 0, 0, 33, 104, 100, 108, 114, 0, 0, 0, 0, 0, 0, 0, 0, 109, 100, 105, 114, 97, 112, 112, 108, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 45, 105, 108, 115, 116, 0, 0, 0, 37, 169, 116, 111, 111, 0, 0, 0, 29, 100, 97, 116, 97, 0, 0, 0, 1, 0, 0, 0, 0, 76, 97, 118, 102, 53, 54, 46, 52, 48, 46, 49, 48, 49])], {type: 'video/mp4'});


const APIKey = 'AIzaSyAqQ0H76XHmXGYwyK8C9DILPgSbBqgPk_k';

function setupDefaultValues (options) {
  return Object.assign({
    muted: false,
    timeout: 250,
    inline: false
  }, options)
}

function startPlayback ({muted, timeout, inline}, elementCallback) {
  let {element, source} = elementCallback()
  let playResult
  let timeoutId
  let sendOutput

  element.muted = muted
  if (muted === true) {
    element.setAttribute('muted', 'muted')
  }
  // indicates that the video is to be played "inline",
  // that is within the element's playback area.
  if (inline === true) {
    element.setAttribute('playsinline', 'playsinline')
  }

  element.src = source

  return new Promise(resolve => {
    playResult = element.play()
    timeoutId = setTimeout(() => {
      sendOutput(false, new Error(`Timeout ${timeout} ms has been reached`))
    }, timeout)
    sendOutput = (result, error = null) => {
      // Clean up to avoid MediaElementLeak
      element.remove()
      element.srcObject = null

      clearTimeout(timeoutId)
      resolve({result, error})
    }

    if (playResult !== undefined) {
      playResult
        .then(() => sendOutput(true))
        .catch(playError => sendOutput(false, playError))
    } else {
      sendOutput(true)
    }
  })
}

//
// API
//

function video (options) {
  options = setupDefaultValues(options)
  return startPlayback(options, () => {
    return {
      element: document.createElement('video'),
      source: URL.createObjectURL(VIDEO)
    }
  })
}

function audio (options) {
  options = setupDefaultValues(options)
  return startPlayback(options, () => {
    return {
      element: document.createElement('audio'),
      source: URL.createObjectURL(AUDIO)
    }
  })
}


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

const model = { players: {}, count: 0, readyCount: 0 };

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
        autoplay: 0,
        loop: 1,
        events: {
            'onReady': event => {
                model.readyCount++;
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
    row.id = 'row-' + ix;
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

// const parseId = link =>
//       link.split('=')[1] || '';

const parseId = link =>
      link.match(/v=.{11}/g)[0].substr(2);

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
        res.push([parseInt(entry.id.substr(4)), parseId(link), parseInt(volume)]);
    }
    return res;
};

function getHashForVideos (videos) {
    return videos.map(([_, videoId, volume]) => videoId + '.' + volume).join(',');
}

async function loadVideoTitles () {
    try {
    for (let playerId in model.players) {
        if (model.players.hasOwnProperty(playerId)) {
            const player = model.players[playerId];
            const videoUrl = player.getVideoUrl();
            if (!videoUrl)
                continue;
            const videoId = parseId(player.getVideoUrl());
            const title = await loadVideoTitle(videoId);
            const container = document.getElementById('row-'+playerId);
            let titleElement = container.querySelectorAll('.video-title')[0];

            if (!titleElement) {
                titleElement = document.createElement('span');
                titleElement.className = 'video-title';
                container.appendChild(titleElement);
            }

            titleElement.textContent = title;
        }
    }
    } catch (e) {
        console.log('e', e);
    }
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
    setTimeout(loadVideoTitles, 1000);
}

function setHandlers () {
    document.querySelector('#add-field').addEventListener('click', () => {
        addField(model.count, '', 100);
        model.count++;
    });
    document.querySelector('#play-button').addEventListener('click', startAllPlayersNow);
    document.querySelector('#pause-button').addEventListener('click', pauseAllPlayers);
}

function startAllPlayers () {
    let tid = setInterval(() => {
        if (model.readyCount == model.count) {
            for (let playerKey in players) {
                if (players.hasOwnProperty(playerKey)) {
                    players[playerKey].playVideo();
                }
            }
            clearInterval(tid);
        }
    }, 10);
}

function awaitAllPlayersReady () {
    return new Promise((resolve, _reject) => {
        if (model.readyCount == model.count) {
            resolve();
            return;
        }

        let tid = setInterval(() => {
            if (model.readyCount == model.count) {
                clearInterval(tid);
                resolve();
            }
        }, 10);
    });
}

function startAllPlayersNow () {
    for (let playerKey in players) {
        if (players.hasOwnProperty(playerKey)) {
            players[playerKey].playVideo();
        }
    }
}

function pauseAllPlayers () {
    for (let playerKey in players) {
        if (players.hasOwnProperty(playerKey)) {
            players[playerKey].pauseVideo();
        }
    }
}

async function init () {
    const videos = parseHash();
    model.count = videos.length;
    putPlayers(videos);
    putFields(videos);
    setHandlers();
    document.getElementById('mix-link').href = document.location.href;
    const canPlay = (await audio()) && (await video());
    if (canPlay) {
        startAllPlayers();
    }
    await awaitAllPlayersReady();
    loadVideoTitles();
}

function memoize(fn) {
    const cache = {};
    return async function() {
        const args = JSON.stringify(arguments);
        cache[args] = cache[args] || fn.apply(undefined, arguments);
        return cache[args];
    };
}

const loadVideoTitle = memoize(async (videoId) => {
    const url = 'https://www.googleapis.com/youtube/v3/videos?part=id%2C+snippet&id=' + videoId + '&key=' + APIKey;
    const response = await fetch(url);
    const json = await response.json();
    console.log(videoId, json);
    return json.items[0].snippet.title;
});

// 2. This code loads the IFrame Player API code asynchronously.
var tag = document.createElement('script');

tag.src = "https://www.youtube.com/iframe_api";
var firstScriptTag = document.getElementsByTagName('script')[0];
firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);

function onYouTubeIframeAPIReady() {
    init();
}
