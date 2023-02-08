import argparse
import os
import requests
import bs4
import subprocess
import difflib

def extract_string(string, comp_pass):
    index = string.find(comp_pass)
    beg = string[index:].find("Abstract syntax:") + len("Abstract syntax:") + index + 1
    end = string[beg:].find("======") + beg
    return string[beg:end]


def process_file(file_path, local, debug, comp_pass):
    result = subprocess.run(["python", f'{local}/compiler.py', file_path], capture_output=True, text=True)
    
    my_output = result.stdout 
    local = extract_string(my_output, comp_pass)
    
    with open(file_path, "r") as file:
        payload = file.read()

    response = requests.post(url, data={"program": payload})
     
    print(f"Sent {file_path}: {response.status_code}")
    
    if response.status_code == 200:
        soup = bs4.BeautifulSoup(response.text, "html.parser")
        tag = soup.find("pre")
        if tag:
            string = tag.text
            online = extract_string(string, comp_pass)
            if debug:
                print("retrieved online compiler string:", online)
                print("retrieved local compiler string:", local)
                
            diff = difflib.ndiff(local.splitlines(), online.splitlines())
        
            print("Differences:")
            if local == online:
                print("The outputs are the same!")
            else:
                result = '\n'.join(list(diff))
                print(result)
        else:
            print("Desired tag not found in response")
    else:
        print(f"Request failed with status code {response.status_code}")


if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("file_or_dir", help="Python File or directory to process")
    parser.add_argument("comp", help="Use the compiler from assignment x, ex: a2")
    parser.add_argument("-v", "--verify", help="Specify a pass to verify | rco | si | ah | pi | pc |", nargs=1, metavar='pass_name')
    parser.add_argument("-l", "--local", help="Specify a local compiler to use", nargs=1, metavar='local_comp')
    parser.add_argument("-d", "--debug", help="Activate debug output", action="store_true")

    args = parser.parse_args()

    comp = args.comp
    local = comp
    
    if (args.local):
        local = args.local[0]

    print("Using online compiler:", comp)
    print("Using local compiler:", local)
        
    url = f'https://jnear.w3.uvm.edu/cs202/compiler-{comp}.php'
    file_or_dir = args.file_or_dir
    debug = args.debug
    comp_pass = "prelude"
    passes = {"rco": "remove complex opera", "si": "select instructions", "ah": "assign homes", \
            "pi": "patch instructions", "pc": "prelude & conclusion"}
 
    if args.verify:
        comp_pass = passes[args.verify[0]]
        print("Pass:", comp_pass)
    
    
    if os.path.isfile(file_or_dir):
        if not file_or_dir.endswith(".py"):
           print("File must end with .py") 
        else:
            process_file(file_or_dir, local, debug, comp_pass)
    elif os.path.isdir(file_or_dir):
        for root, dirs, files in os.walk(file_or_dir):
            for filename in files:
                file_path = os.path.join(root, filename)
                if file_path.endswith(".py"):
                    process_file(file_path, local, debug, comp_pass)
    else:
        print(f"Error: {file_or_dir} is not a file or directory")
