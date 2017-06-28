#include <fstream>
#include <iostream>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <inttypes.h>

#include "diskhash.hpp"

int main(int argc, char** argv) {
    if (argc < 2) {
        std::cerr << "Usage:\n"
            << argv[0] << " [sub-command] [ARGS...]\n";
        return 1;
    }
    std::string mode = argv[1];
    if (mode == "create") {
        if (argc < 5 || std::atol(argv[3]) <= 0) {
            std::cerr << "Usage:\n"
                << argv[0] << " create FILE.dht key-size input-file\n";
            return 1;
        }
        const size_t key_maxlen = std::atol(argv[3]);
        dht::DiskHash<uint64_t> ht(argv[2], key_maxlen, dht::DHOpenRW);
        std::string line;
        std::ifstream finput(argv[4]);
        uint64_t ix = 0;
        while (std::getline(finput, line)) {
            if (line.length() > key_maxlen) {
                std::cerr << "Key too long: '" << line << "'. Aborting.\n";
                return 2;
            }
            const bool inserted = ht.insert(line.c_str(), ix);
            if (!inserted) {
                std::cerr  << "Found repeated key '" << line << "' (ignored).\n";
            }
            ++ix;
        }
    } else if (mode == "lookup") {
        if (argc < 5 || std::atol(argv[3]) < 0) {
            std::cerr << "Usage:\n"
                << argv[0] << " lookup FILE.dht key-size input-file\n";
            return 1;
        }
        const size_t key_maxlen = std::atol(argv[3]);
        dht::DiskHash<uint64_t> ht(argv[2], key_maxlen, dht::DHOpenRO);
        std::string line;
        std::ifstream finput(argv[4]);
        while (std::getline(finput, line)) {
            if (line.length() > key_maxlen) {
                std::cout << "-1\n";
            } else {
                const uint64_t* val = ht.lookup(line.c_str());
                std::cout << (val ? *val : -1) << '\n';
            }
        }

    } else {
        std::cerr << "Unknown subcommand: '" << mode << "'\n";
        return 1;
    }
    return 0;
}

