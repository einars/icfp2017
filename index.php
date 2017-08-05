<?php

require_once('/services/web/localhost/stitches/stitches.php');

page::add_stylesheet('https://maxcdn.bootstrapcdn.com/bootstrap/3.3.7/css/bootstrap.min.css');
page::add_script('/punt/sorttable.js');
s::add_routes([
    '' => 'on_punt_index',
]);

s::run();

function add_css()
{
    echo <<<CSS
<style>
.w100 { width: 100% }
.w0 { width: auto !important }
.btn.w100 { text-align: left }
</style>
CSS;
}

function add_js()
{
    page::add_script('http://code.jquery.com/jquery-3.2.1.min.js');
    echo <<<SCRIPT
<script>
const refresh = () => {
    $('.js-ports').load('?action=ports');
}

$(() => {

    const refresht = () => {
        refresh();
        window.setTimeout(refresht, 20000);
    }

    refresht();


    $('.js-refresh').click(refresh);

    $(document).on('click', '.js-kill', function (ev) {
        ev.preventDefault();

        const d = {
            port: $(this).data('port'),
        }
        
        $.post('?action=kill', d, function (res) {
            if (res != 'OK') {
                alert('Something unexpected happened.');
            } else {
                refresh();
                alert('Server probably killed.');
            }
        })
    });
    $('.js-run').click(function (ev) {
        ev.preventDefault();

        const d = {
            map: $(this).data('map'),
            players: $('.js-players').val(),
            port: $('.js-port').val(),
        }
        
        $.post('?action=run', d, function (res) {
            if (res != 'OK') {
                alert('Something unexpected happened.');
            } else {
                refresh();
                alert('Server probably started.');
            }
        })
    });
});
</script>
SCRIPT;
}

function get_maps()
{
    $files = glob('/proj/svunt/maps/**/*.json');
    $out = [];
    foreach($files as $f) {
        $safe_name = safe_name(basename(substr($f, 0, -4)));
        $json = json_decode(file_get_contents($f), $assoc = true);
        $out[$safe_name] = [
            'full_path' => $f,
            'name' => basename($f),
            'size' => filesize($f),
            'short' => $safe_name,
            'sites' => sizeof(get('sites', $json)),
            'rivers' => sizeof(get('rivers', $json)),
            'mines' => sizeof(get('mines', $json)),
        ];
    }
    return $out;
}

function get_ports()
{
    ob_start();
    system('sudo /usr/bin/netstat -plnt  2>&1 | grep main.native');
    $data = ob_get_clean();
    $out = [];
    foreach(explode("\n", $data) as $line) {
        if ( ! $line) continue;
        $pts = preg_split('/\s+/', $line);
        $ip_port = $pts[3];
        $pid_prog = $pts[6];

        list($pid, $prog) = explode('/', $pid_prog);

        list($_, $port) = explode(':', $ip_port);

        $out[$port] = [
            'port' => $port,
            'pid' => $pid,
            'name' => $pid_prog,
            'cmdline' => str_replace("\x00", " ", file_get_contents("/proc/$pid/cmdline")),
        ];

        
    }
    asort($out);
    return $out;
}

function on_punt_index()
{
    if (get('action') == 'run') {
        return on_run();
    }
    if (get('action') == 'kill') {
        return on_kill();
    }
    if (get('action') == 'ports') {
        ob_end_clean();
        draw_ports();
        die();
    }

    add_css();
    add_js();
    echo '<div class="container-fluid">';

    echo '<div class="col-md-6 col-lg-6">';

    $map = get('map');
    $maps = get_maps();

    $ports = get_ports();

    HTML::h2('Punt!');

    HTML::select('players', get('players', $_REQUEST, 2), [
        'class' => 'form-control js-players',
        'choices' => [
            '1' => '1 player',
            '2' => '2 players',
            '3' => '3 players',
            '4' => '4 players',
        ]
    ]);

    for($port = 6000; $port < 6100; $port++) {
        if ( ! isset($ports[$port])) {
            break;
        }
    }



    HTML::input('port', get('port', $_REQUEST, $port), [
        'class' => 'form-control js-port',
        'choices' => [
            '1' => '1 player',
            '2' => '2 players',
            '3' => '3 players',
            '4' => '4 players',
        ]
    ]);


    HTML::h2('Available maps');

    echo '<table class="table sortable">';
    echo '<thead><tr>
    <th>Map</th>
    <th>Size</th>
    <th>Sites</th>
    <th>Rivers</th>
    <th>Mines</th>
    <th>Actions</th>
    </tr></thead>';
    foreach($maps as $k=>$m) {
        echo '<tr>';

        h('<td>%s</td>', $m['name']);
        h('<td class="text-right">%s</td>', $m['size']);
        h('<td class="text-right">%d</td>', $m['sites']);
        h('<td class="text-right">%d</td>', $m['rivers']);
        h('<td class="text-right">%d</td>', $m['mines']);

        echo '<td>';
        HTML::button("Run {$m['short']}", [ 'class' => 'btn btn-success w100 js-run', 'data-map' => $m['short']]);
        echo '</td>';

        echo '</tr>';
    }
    echo '</table>';
    echo '</div>'; //col-md-6

    echo '<div class="col-md-6">';
    echo '<div class="js-ports">';
    echo '</div>';
    echo '<button type="button" class="btn btn-info js-refresh">Refresh processlist</button>';
    echo '</div>';


    echo '</div>'; // container-fluid
}

function draw_ports($ports = null)
{
    if ( ! $ports) $ports = get_ports();
    html::h2('Running processes <span class="text-muted" style="font-size: 12px">' . date('H:m:s') . '</span>');
    echo '<table class="table sortable">';
    echo '<thead><tr>
        <th>Port</th>
        <th>PID</th>
        <th>Commandline</th>
    </tr></thead>';
    foreach($ports as $p) {
        echo '<tr>';
        h('<td>%s</td>', $p['port']);
        h('<td>%s</td>', $p['pid']);
        h('<td>%s</td>', $p['cmdline']);
        h('<td><button data-port="%s" class="js-kill btn btn-danger">kill</button></td>', $p['port']);
        echo '</tr>';
    }
    echo '</table>';

}

function on_kill()
{
    ob_end_clean();
    $ports = get_ports();
    $port = get('port');
    if (isset($ports[$port])) {
        $pid = $ports[$port]['pid'];
        // exec("sudo /usr/bin/kill $pid");
        exec("sudo /usr/bin/kill $pid");
        die('OK');
    } else {
        die("NOK $port");
    }
}

function on_run() 
{
    ob_end_clean();
    $maps = get_maps();
    $port = get('port');
    $map = get('map');
    $players = get('players');
    $cmd = sprintf('/proj/svunt/run-x %d %d %s',
        $port,
        $players,
        escapeshellarg(get('full_path', get($map, $maps, []))));

    exec("$cmd >/dev/null 2>/dev/null &");
    die('OK');
}
