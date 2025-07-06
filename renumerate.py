import os
import sys
import subprocess


def renumerate(file_path: str) -> str:
    number, other_name = file_path[:2], file_path[2:]
    next_number = ("0" + str(int(number) + 1))[-2:]
    return next_number + other_name


def renumerate_modules(directory_path, start_module):
    try:
        directories = sorted(
            [
                item
                for item in os.listdir(directory_path)
                if os.path.isdir(os.path.join(directory_path, item))
            ]
        )

        print("Переименованные модули")
        for directory in directories:
            module_number = directory[:2]
            if str.isnumeric(module_number) and int(module_number) >= int(start_module):
                full_directory = directory_path + directory
                new_directory = directory_path + renumerate(directory)
                print(f"{full_directory} -> {new_directory}")
                subprocess.run(["git", "mv", full_directory, new_directory])

    except FileNotFoundError:
        print(f"Ошибка: Папка '{directory_path}' не найдена.")
    except PermissionError:
        print(f"Ошибка: Нет доступа к папке '{directory_path}'.")
    except Exception as e:
        print(f"Произошла ошибка: {e}")


renumerate_modules(sys.argv[1], sys.argv[2])
