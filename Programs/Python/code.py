import subprocess
import csv
import sys
from datetime import datetime

def scan_wifi():
    
    try:
        proc = subprocess.run(["nmcli", "-t", "-f", "SSID,SECURITY,BSSID,SIGNAL,CHAN", "dev", "wifi"], capture_output=True, text=True)
    except FileNotFoundError:
        print("Error: nmcli 0no encontrado. Instala NetworkManager/nmcli o ejecuta en un disco con nmcli.", file=sys.stderr)
    except subprocess.CalledProcessError as e:
        print("Error ejecutando nmcli", e, file=sys.stderr)
        sys.exit(1)
    lines = proc.stdout.strip().splitlines()
    entries= []
    seen = set()
    for line in lines:
        parts    = line.split(":")
        ssid     = parts[0].strip() if len(parts) > 0 else ""
        security = parts[1].strip() if len(parts) > 1 else ""
        bssid    = parts[2].strip() if len(parts) > 2 else ""
        signal   = parts[3].strip() if len(parts) > 3 else ""
        chan     = parts[4].strip() if len(parts) > 4 else ""

        key = (ssid, bssid)
        if key in seen:
            continue
        seen.add(key)

        nmo qui

