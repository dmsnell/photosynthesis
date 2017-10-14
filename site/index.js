var app = Elm.Main.fullscreen()

function requestNextPage() {
	app.ports.requestNextPage.send(null)
}

var lastScroll = 0
function onScroll() {
	var scroll = window.scrollY + window.innerHeight
	var height = document.body.scrollHeight

	if ( scroll > lastScroll && scroll > height * 0.9 ) {
		requestNextPage()
	}

	lastScroll = scroll
}

window.requestNextPage = requestNextPage
window.addEventListener( 'scroll', onScroll, { passive: true } )