const APIKey = 'AIzaSyAqQ0H76XHmXGYwyK8C9DILPgSbBqgPk_k';

window.dbg = { players: [] };

exports.newYouTubePlayer =
    elementId => videoId => volume => size => () => {
        console.log('newYouTubePlayer', elementId, videoId);
        const element = document.getElementById(elementId);
        window.dbg[elementId] = element;

        const player = new YT.Player(element, {
            height: size.height,
            width: size.width,
            videoId: videoId,
            playerVars: {
                'playsinline': 1
            },
            enablejsapi: 1,
            autoplay: 1,
            volume: 0,
            loop: 1,
            events: {
                'onReady': event => {
                    player.setLoop(true);
                    player.setVolume(volume);
                    player.playVideo();
                },
                'onStateChange': event => {
                    if (event.data == YT.PlayerState.ENDED) {
                        player.playVideo(0);
                    }
                }
            }
        });
        player._my_videoId = videoId;
        player._my_elementId = elementId;
        window.dbg.players.push(player);
        return player;
    };

exports.setOpacity = elementId => opacity => () => {
    document.getElementById(elementId).parentNode.style.opacity = opacity;
};

exports.setSize = player => width => height => () => {
    player.setSize(width, height);
};

exports.destroyPlayer = player => () => {
    console.log('destroying', player._my_elementId, player._my_videoId);
    player.destroy();
};

exports.pausePlayer = player => () => {
    player.pauseVideo();
};

exports.resumePlayer = player => () => {
    player.playVideo();
};

exports.loadVideoById = player => videoId => () => {
    if (typeof player.loadVideoById == 'function') {
        player.loadVideoById(videoId);
    }
};

exports.setVolume = player => volume => () => {
    if (typeof player.setVolume == 'function') {
        player.setVolume(volume);
    }
};

exports.getVolume = player => () => {
    try {
        return player.getVolume();
    } catch (_e) {
        return 0;
    }
};

const memoize = (fn) => {
    const cache = {};
    return async function() {
        const args = JSON.stringify(arguments);
        cache[args] = cache[args] || fn.apply(undefined, arguments);
        return cache[args];
    };
}

const loadVideoTitleMemoized = // memoize(
      just => nothing => videoId =>
        new Promise((resolve, reject) => {
            const url = 'https://www.googleapis.com/youtube/v3/videos?part=id%2C+snippet&id=' + videoId + '&key=' + APIKey;
            fetch(url)
                .then(response => response.json())
                .then(json => {
                    console.log(json);
                    let title = json.items[0].snippet.title;
                    resolve(title ? just(title) : nothing);
                }).catch(_ => resolve(nothing));
        });

exports.loadVideoTitle_ = just => nothing => videoId => () =>
    loadVideoTitleMemoized(just)(nothing)(videoId);
