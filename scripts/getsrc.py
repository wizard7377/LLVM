# Gets all the source files for `llvm-idr.ipkg`
import glob

files0 = glob.glob('./llvm-idr/src/**/*.idr', recursive=True)
files1 = [file[15:-4] for file in files0]
files2 = [file.replace('/','.') for file in files1]
print("\n\n========Source Files========\n\n")
for file in files2:
    print(file + ",")

print("\n\n=========Test Files=========\n\n")

files3 = glob.glob('./llvm-test/src/**/*.idr', recursive=True)
files4 = [file[16:-4] for file in files3]
files5 = [file.replace('/','.') for file in files4]
for file in files5:
    print(file + ",")