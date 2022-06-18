let resolveYT = () => {};

window.onYouTubeIframeAPIReady = () => {
    console.log('YT loaded');
    resolveYT();
};

exports.loadYouTubeAPI_ = () => {
    return new Promise ((resolve, reject) => {
        resolveYT = resolve;
        var tag = document.createElement('script');
        tag.src = "https://www.youtube.com/iframe_api";
        var firstScriptTag = document.getElementsByTagName('script')[0];
        firstScriptTag.parentNode.insertBefore(tag, firstScriptTag);
    });
};
