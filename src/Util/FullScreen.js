exports.listenForFullscreenExit = cb => () => {
    if (document.addEventListener)
    {
        document.addEventListener('fullscreenchange', exitHandler, false);
        document.addEventListener('mozfullscreenchange', exitHandler, false);
        document.addEventListener('MSFullscreenChange', exitHandler, false);
        document.addEventListener('webkitfullscreenchange', exitHandler, false);
    }

    function exitHandler()
    {
        if (!document.webkitIsFullScreen && !document.mozFullScreen && !document.msFullscreenElement)
        {
            cb();
        }
    }
};
