// Mostrar letra sincronizada
const lyricsEl = document.getElementById('lyrics');
let lrcLines = [];

function parseLRC(lrcText) {
    const lines = lrcText.split(/\r?\n/);
    const result = [];
    const timeExp = /\[(\d{2}):(\d{2}\.\d{2})\]/;
    for (const line of lines) {
        const match = timeExp.exec(line);
        if (match) {
            const min = parseInt(match[1]);
            const sec = parseFloat(match[2]);
            const time = min * 60 + sec;
            const text = line.replace(timeExp, '').trim();
            if (text) result.push({ time, text });
        }
    }
    return result;
}

function showLyrics() {
    if (!lrcLines.length) {
        lyricsEl.innerHTML = '<i>No hay letra disponible</i>';
        return;
    }
    lyricsEl.innerHTML = lrcLines.map(l => `<div>${l.text}</div>`).join('');
}

function syncLyrics(currentTime) {
    if (!lrcLines.length) return;
    let activeIdx = -1;
    for (let i = 0; i < lrcLines.length; i++) {
        if (currentTime >= lrcLines[i].time) activeIdx = i;
    }
    // Resalta la línea activa usando atributo [active]
    lyricsEl.innerHTML = lrcLines.map((l, idx) =>
        `<div${idx === activeIdx ? ' active' : ''}>${l.text}</div>`
    ).join('');
    // Scroll automático para la línea activa
    if (activeIdx >= 0) {
        const activeLine = lyricsEl.querySelector('div[active]');
        if (activeLine) {
            activeLine.scrollIntoView({ behavior: 'smooth', block: 'center' });
        }
    }
}
let songs = [];
fetch('music/index_music.json')
    .then(response => response.json())
    .then(data => {
        Object.values(data).forEach(genero => {
            Object.values(genero).forEach(artista => {
                artista.forEach(cancion => {
                    songs.push({
                        title: cancion.titulo,
                        artist: cancion.artista,
                        src: "music/" + cancion.archivo,
                        cover: cancion.cover && cancion.cover !== "" ? cancion.cover : "https://i.imgur.com/8Km9tLL.jpg",
                        letra_lrc: cancion.letra_lrc
                    });
                });
            });
        });
        renderSongList();
        loadSong(currentSong);
    });

const songListEl = document.getElementById('song-list');
function renderSongList() {
    songListEl.innerHTML = '<b>Lista de canciones:</b><ul style="padding-left:16px;">' +
        songs.map((s, i) => `<li style='margin-bottom:8px;cursor:pointer;color:#a084e8;' onclick='selectSong(${i})'>${s.title} - ${s.artist}</li>`).join('') + '</ul>';
}
window.selectSong = function(idx) {
    currentSong = idx;
    loadSong(currentSong);
    playSong();
}
renderSongList();
let currentSong = 0;
const audio = document.getElementById('audio');
const playBtn = document.getElementById('play');
const prevBtn = document.getElementById('prev');
const nextBtn = document.getElementById('next');
const progressBar = document.getElementById('progress-bar');
const progress = document.getElementById('progress');
const currentTimeEl = document.getElementById('current-time');
const durationEl = document.getElementById('duration');
const titleEl = document.getElementById('song-title');
const artistEl = document.getElementById('song-artist');
const coverEl = document.getElementById('cover');

function loadSong(index) {
    const song = songs[index];
    audio.src = song.src;
    titleEl.textContent = song.title;
    artistEl.textContent = song.artist;
    coverEl.src = song.cover && song.cover !== "" ? song.cover : "https://i.imgur.com/8Km9tLL.jpg";
    lrcLines = [];
    if (song.letra_lrc) {
        lrcLines = parseLRC(song.letra_lrc);
    }
    showLyrics();
}

function playSong() {
    audio.play();
    playBtn.innerHTML = '&#10073;&#10073;'; // Pause icon
}

function pauseSong() {
    audio.pause();
    playBtn.innerHTML = '&#9654;'; // Play icon
}

playBtn.addEventListener('click', () => {
    if (audio.paused) {
        playSong();
    } else {
        pauseSong();
    }
});

prevBtn.addEventListener('click', () => {
    currentSong = (currentSong - 1 + songs.length) % songs.length;
    loadSong(currentSong);
    playSong();
});

nextBtn.addEventListener('click', () => {
    currentSong = (currentSong + 1) % songs.length;
    loadSong(currentSong);
    playSong();
});

audio.addEventListener('loadedmetadata', () => {
    durationEl.textContent = formatTime(audio.duration);
});

audio.addEventListener('timeupdate', () => {
    currentTimeEl.textContent = formatTime(audio.currentTime);
    const percent = (audio.currentTime / audio.duration) * 100;
    progress.style.width = percent + '%';
    syncLyrics(audio.currentTime);
});

// Reproducir siguiente canción automáticamente al terminar
audio.addEventListener('ended', () => {
    currentSong = (currentSong + 1) % songs.length;
    loadSong(currentSong);
    playSong();
});

progressBar.addEventListener('click', (e) => {
    const rect = progressBar.getBoundingClientRect();
    const x = e.clientX - rect.left;
    const percent = x / rect.width;
    audio.currentTime = percent * audio.duration;
});

function formatTime(seconds) {
    if (isNaN(seconds)) return '00:00';
    const m = Math.floor(seconds / 60);
    const s = Math.floor(seconds % 60);
    return `${m.toString().padStart(2, '0')}:${s.toString().padStart(2, '0')}`;
}

// Inicializa el reproductor
loadSong(currentSong);
