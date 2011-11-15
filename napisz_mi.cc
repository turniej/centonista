// Generates poems based on the resources of wolnelektury.pl.

// Copyright (C) 2011  Marcin Ciura (mciura@gmail.com)
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU Affero General Public License as
// published by the Free Software Foundation, either version 3 of the
// License, or (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU Affero General Public License for more details.
//
// You should have received a copy of the GNU Affero General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.

#include <assert.h>
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <algorithm>
#include <iterator>
#include <map>
#include <set>
#include <string>
#include <utility>
#include <vector>

namespace centonista {

const char kModelFileName[] = "model.d";
const char kRhymeFileName[] = "rymy.d";
const char kTemplateFileName[] = "szablony.d";
const char kWordFileName[] = "zestroje.d";
const char kRandomNumberSource[] = "/dev/urandom";

typedef int Position;
typedef int Rhythm;
typedef int RhymeId;
typedef int WordId;

typedef std::map<Rhythm, std::vector<WordId>*> SuccessorMap;

const Position kStartPosition = 0;
const Rhythm kNullRhythm = 0;
const WordId kPeriod = 0;

void Fatal(const char* format, ...) {
  va_list ap;
  va_start(ap, format);
  vfprintf(stderr, format, ap);
  fprintf(stderr, "\n");
  va_end(ap);
  exit(EXIT_FAILURE);
}

std::string StringPrintf(const char* format, ...) {
  char buffer[8192];
  va_list ap;
  va_start(ap, format);
  vsnprintf(buffer, sizeof buffer, format, ap);
  va_end(ap);
  return buffer;
}

FILE* OpenFile(const char* filename) {
  FILE* file = fopen(filename, "r");
  if (file == NULL)
    Fatal("Nie mogę otworzyć pliku %s", filename);
  return file;
}

const char kUtf8Lowercase[] = "ąáâäćçčéęëíîłńóôöśšúüýźžż";
const char kUtf8Uppercase[] = "ĄÁÂÄĆÇČÉĘËÍÎŁŃÓÔÖŚŠÚÜÝŹŽŻ";

void Capitalize(std::string* word_form) {
  if (word_form->empty())
    return;
  if (word_form->at(0) >= 'a' && word_form->at(0) <= 'z') {
    word_form->at(0) += ('A' - 'a');
    return;
  }
  if (word_form->size() == 1)
    return;
  for (int i = 0; kUtf8Lowercase[i] != '\0'; i += 2) {
    if (word_form->at(0) == kUtf8Lowercase[i] &&
        word_form->at(1) == kUtf8Lowercase[i + 1]) {
      word_form->at(0) = kUtf8Uppercase[i];
      word_form->at(1) = kUtf8Uppercase[i + 1];
      break;
    }
  }
}

void Lowercase(std::string* word_form) {
  for (int i = 0; i < word_form->size(); ++i) {
    if (word_form->at(i) >= 'A' && word_form->at(i) <= 'Z') {
      word_form->at(i) += ('a' - 'A');
    } else if (static_cast<unsigned char>(word_form->at(i)) >= 128) {
      for (int j = 0; kUtf8Uppercase[j] != '\0'; j += 2) {
        if (word_form->at(i) == kUtf8Uppercase[j] &&
            word_form->at(i + 1) == kUtf8Uppercase[j + 1]) {
          word_form->at(i) = kUtf8Lowercase[j];
          word_form->at(i + 1) = kUtf8Lowercase[j + 1];
          ++i;
        }
      }
    }
  }
}

bool StripPrefixIfPresent(const char* prefix, std::string* s) {
  int i = 0;
  for (/**/; prefix[i] != '\0'; ++i) {
    if (s->size() <= i || s->at(i) != prefix[i])
      return false;
  }
  *s = s->substr(i);
  return true;
}

class Random {
 public:
  Random() {
    FILE* dev_random = OpenFile(kRandomNumberSource);
    if (fread(seed_, sizeof seed_[0], 3, dev_random) != 3)
      Fatal("Nie udał się odczyt z pliku %s", kRandomNumberSource);
    fclose(dev_random);
  }

  ~Random() {}

  std::string GetSeed() const {
    return StringPrintf("%04x%04x%04x", seed_[0], seed_[1], seed_[2]);
  }

  bool SetSeed(const std::string& s) {
    unsigned short tmp[3];
    if (s.size() != 12)
      return false;
    if (sscanf(s.c_str(), "%04hx%04hx%04hx", &tmp[0], &tmp[1], &tmp[2]) != 3)
      return false;
    memcpy(seed_, tmp, sizeof tmp);
    return true;
  }

  template <typename T>
  int GetRandomIndex(const std::vector<T>& v) {
    assert(!v.empty());
    return nrand48(seed_) % v.size();
  }

 private:
  unsigned short seed_[3];

  Random(const Random&);
  void operator=(const Random&);
};

Rhythm EncodeRhythm(const std::string& rhythm) {
  Rhythm result = (1 << rhythm.size()) - 1;
  for (Position position = 0; position < rhythm.size(); ++position) {
    assert(rhythm[position] == '.' || rhythm[position] == '*');
    if (rhythm[position] == '*')
      result += 1 << position;
  }
  return result;
}

std::string DecodeRhythm(Rhythm rhythm) {
  ++rhythm;
  std::string result;
  for (Rhythm m = rhythm / 2; m != 0; m /= 2) {
    result.push_back('.');
  }
  for (int i = 0; i < result.size(); ++i) {
    if (rhythm % 2)
      result[i] = '*';
    rhythm /= 2;
  }
  return result;
}

// http://graphics.stanford.edu/~seander/bithacks.html#IntegerLog
int GetNumSyllables(Rhythm rhythm) {
  int result = 0;
  ++rhythm;
  if (rhythm & 0xFFFF0000) {
    rhythm >>= 16;
    result |= 16;
  }
  if (rhythm & 0xFF00) {
    rhythm >>= 8;
    result |= 8;
  }
  if (rhythm & 0xF0) {
    rhythm >>= 4;
    result |= 4;
  }
  if (rhythm & 0xC) {
    rhythm >>= 2;
    result |= 2;
  }
  if (rhythm & 0x2) {
    rhythm >>= 1;
    result |= 1;
  }
  return result;
}

// Note that pattern is not passed by reference.
void ExpandQuestionMarks(std::string pattern,
                         std::vector<std::string>* result) {
  std::vector<int> question_mark_positions;
  for (int i = 0; i < pattern.size(); ++i) {
    if (pattern[i] == '?')
      question_mark_positions.push_back(i);
  }
  result->clear();
  const int upper_limit = 1 << question_mark_positions.size();
  for (int i = 0; i < upper_limit; ++i) {
    int copy_i = i;
    for (int j = 0; j < question_mark_positions.size(); ++j) {
      pattern[question_mark_positions[j]] = (copy_i % 2)[".*"];
      copy_i /= 2;
    }
    result->push_back(pattern);
  }
}

const signed char kBase32Table[256] = {
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 62, -1, -1, -1, 63,
  +0,  1,  2,  3,  4,  5,  6,  7,  8,  9, -1, -1, -1, -1, -1, -1,
  -1, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24,
  25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, -1, -1, -1, -1, -1,
  -1, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50,
  51, 52, 53, 54, 55, 56, 57, 58, 59, 60, 61, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
  -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,
};

class Base32Reader {
 public:
  explicit Base32Reader(const char* s) : s_(s) {}
  ~Base32Reader() {}

  int Read() {
    int result = 0;
    while (true) {
      int d = kBase32Table[static_cast<unsigned char>(*s_)];
      if (d < 0)
        break;
      ++s_;
      if (d >= 32)
        return 32 * result + (d - 32);
      else
        result = 32 * result + d;
    }
    return -1;
  }

  char peek() const { return *s_; }

 private:
  const char* s_;

  Base32Reader(const Base32Reader&);
  void operator=(const Base32Reader&);
};

class Word {
 public:
  Word(const std::string& form, Rhythm rhythm, RhymeId rhyme_id)
    : form_(form), rhythm_(rhythm), rhyme_id_(rhyme_id) {}
  ~Word() {}

  const std::string& form() const { return form_; }
  Rhythm rhythm() const { return rhythm_; }
  RhymeId rhyme_id() const { return rhyme_id_; }

 private:
  std::string form_;
  Rhythm rhythm_;
  RhymeId rhyme_id_;
};

class Model {
 public:
  Model() {}
  ~Model() {}

  void Init() {
    // awk '{if(length>x)x=length}END{print x/1024}' < model.d prints about 60.
    char line[96 * 1024];
    FILE* word_file = OpenFile(kWordFileName);
    words_.reserve(512 * 1024);
    RhymeId max_rhyme_id = 0;
    while (fgets(line, sizeof line, word_file) != NULL) {
      char* p = strchr(line, ' ');
      if (p == NULL)
        Fatal("Błąd w pliku %s", kWordFileName);
      *p = '\0';
      Base32Reader reader(p + 1);
      const Rhythm rhythm = reader.Read();
      const RhymeId rhyme_id = reader.Read();
      if (rhythm < 0 || rhyme_id < 0 || reader.peek() != '\n')
        Fatal("Błąd w pliku %s", kWordFileName);
      std::replace(line, p, '_', ' ');
      words_.push_back(Word(line, rhythm, rhyme_id));
      if (rhyme_id >= max_rhyme_id)
        max_rhyme_id = rhyme_id;
    }
    fclose(word_file);
    words_.push_back(Word("ROOT", 0, 0));
    num_rhymes_ = max_rhyme_id + 1;

    FILE* model_file = OpenFile(kModelFileName);
    successor_maps_.reserve(words_.size());
    SuccessorMap* root_map = new SuccessorMap;
    while (fgets(line, sizeof line, model_file) != NULL) {
      SuccessorMap* m = new SuccessorMap;
      Base32Reader reader(line);
      WordId word_id = 0;
      while (true) {
        const int delta = reader.Read();
        if (delta < 0)
          break;
        word_id += delta;
        Rhythm rhythm = word(word_id).rhythm();

        SuccessorMap::iterator it = m->find(rhythm);
        if (it == m->end())
          it = m->insert(make_pair(rhythm, new std::vector<WordId>)).first;
        it->second->push_back(word_id);

        it = root_map->find(rhythm);
        if (it == root_map->end())
          it = root_map->insert(make_pair(rhythm, new std::vector<WordId>)).first;
        it->second->push_back(word_id);
      }
      if (m->empty() || reader.peek() != '\n')
        Fatal("Błąd w pliku %s", kModelFileName);
      successor_maps_.push_back(m);
    }
    successor_maps_.push_back(root_map);
    fclose(model_file);

    FILE* rhyme_file = OpenFile(kRhymeFileName);
    int i = 0;
    while (fgets(line, sizeof line, rhyme_file) != NULL) {
      line[strlen(line) - 1] = '\0';
      rhyme_ids_[line] = i;
      ++i;
    }
    fclose(rhyme_file);
  }

  const Word& word(WordId word_id) const { return words_.at(word_id); }
  const SuccessorMap& successor_map(WordId word_id) const {
    return *successor_maps_.at(word_id);
  }
  WordId root_id() const { return words_.size() - 1; }
  RhymeId num_rhymes() const { return num_rhymes_; }
  RhymeId rhyme_id(const std::string& s) const {
    std::map<std::string, RhymeId>::const_iterator it = rhyme_ids_.find(s);
    return (it == rhyme_ids_.end()) ? -1 : it->second;
  }

 private:
  std::vector<Word> words_;
  std::vector<SuccessorMap*> successor_maps_;
  std::map<std::string, RhymeId> rhyme_ids_;
  RhymeId num_rhymes_;

  Model(const Model&);
  void operator=(const Model&);
};

bool IsPeriodAmongSuccessors(const SuccessorMap& successor_map) {
  const SuccessorMap::const_iterator it = successor_map.find(kNullRhythm);
  return (it != successor_map.end() && it->second->at(0) == kPeriod);
}

class WordPosition {
 public:
  WordPosition(WordId word_id, Position position)
      : encoded_((position << 22) + word_id) {}
  ~WordPosition() {}

  WordId word_id() const { return encoded_ % (1 << 22); }
  Position position() const { return encoded_ >> 22; }

  bool operator<(const WordPosition& other) const {
    return encoded_ < other.encoded_;
  }

 private:
  unsigned encoded_;
};

bool RhymesAreTrivial(const std::vector<WordPosition>& rhymes,
                      int threshold,
                      const Model& model) {
  // "naj" should precede "na", etc.
  static const char* kPrefixes[] = {
    "bez", "naj", "nie",
    "do", "nade", "nad", "na", "obe", "ob", "ode", "od", "o",
    "pode", "pod", "po", "prze", "przy", "roze", "roz", "s", "ś",
    "u", "we", "ws", "wy", "wze", "wz", "w", "za", "ze", "z",
    "pięć", "sześć", "siedem", "siedm", "osiem", "ośm", "dziewięć",
    "dwu", "trzy", "cztero", "pięcio", "sześcio", "siedmio", "ośmio",
    "dwój", "trój", "czwór",
    "jede", "czter", "pięt", "szes", "dziewięt",
    "obu", "kilka", "kilku", "pół", "spół", "współ",
    "ma", "twa", "swa",
    "mą", "twą", "swą",
    "me", "twe", "swe",
    "mo", "two", "swo",
    "mój", "twój", "swój",
    "mym", "twym", "swym",
    "nas", "was",
    "ciebi", "siebi", "tobi", "sobi",
    "mało", "wielo", "jasno", "ciemno", "równo", "różno",
    "arcy", "eks", "krypto",
    NULL,
  };
  int size = rhymes.size();
  if (size < threshold)
    return true;
  std::vector<std::string> codas;
  int checked_rhymes = 0;
  for (int i = 0; i < rhymes.size(); ++i) {
    std::string coda = model.word(rhymes[i].word_id()).form();
    const int last_underscore = coda.find_last_of(" -'");
    if (last_underscore != std::string::npos)
      coda = coda.substr(last_underscore + 1);
    Lowercase(&coda);
    if (coda.size() > 3 &&
        coda[coda.size() - 1] == 'j' &&
        coda[coda.size() - 2] == 'e') {
      coda.resize(coda.size() - 2);
      if (coda[coda.size() - 1] != 'i')
        coda += 'y';
    }
    if (coda.size() > 2 &&
        coda[coda.size() - 1] == '\x85' &&
        coda[coda.size() - 2] == '\xC4') {  // piękną -> piękno
      coda.resize(coda.size() - 2);
      coda += 'o';
    }
    if (coda.size() > 2 &&
        coda[coda.size() - 1] == '\x99' &&
        coda[coda.size() - 2] == '\xC4') {  // żyję -> żyje
      coda.resize(coda.size() - 2);
      coda += 'e';
    }
    std::string unstripped = coda;
    bool has_been_stripped = false;
    for (int i = 0; kPrefixes[i] != NULL; ++i) {
      if (StripPrefixIfPresent(kPrefixes[i], &coda)) {
        has_been_stripped = true;
        break;
      }
    }
    bool found = (std::find(codas.begin(), codas.end(), coda) != codas.end());
    if (!found && has_been_stripped)
      found = (std::find(codas.begin(), codas.end(), unstripped) !=
               codas.end());
    if (found) {
      --size;
      if (size < threshold)
        return true;
    } else if (checked_rhymes >= threshold - 1) {
      return false;
    } else {
      codas.push_back(coda);
      ++checked_rhymes;
      if (has_been_stripped)
        codas.push_back(unstripped);
    }
  }
  return false;
}

typedef std::map<WordPosition, std::vector<WordPosition> > Trellis;

class Verse {
 public:
  explicit Verse(const std::string& s) : initialized_(false) {
    std::string syllables;
    for (int i = 0; i < s.size(); ++i) {
      if (s[i] == '.' || s[i] == '*' || s[i] == '?')
        syllables.push_back(s[i]);
      else if (s[i] == '|')
        breaks_.push_back(syllables.size());
      else
        Fatal("Wciórności");
    }
    num_syllables_ = syllables.size();
    breaks_.push_back(num_syllables());
    continuations_.resize(num_syllables() + 1);
    for (Position position = kStartPosition; position < num_syllables();
         ++position) {
      const std::vector<Position>::const_iterator next_break =
          upper_bound(breaks_.begin(), breaks_.end(), position);
      std::vector<Rhythm>& rhythms = continuations_.at(position);
      rhythms.push_back(kNullRhythm);
      std::vector<std::string> rhythm_patterns;
      const int max_length = *next_break - position;
      for (int length = 1; length <= max_length; ++length) {
        ExpandQuestionMarks(
            syllables.substr(position, length), &rhythm_patterns);
        for (int i = 0; i < rhythm_patterns.size(); ++i) {
          rhythms.push_back(EncodeRhythm(rhythm_patterns[i]));
        }
      }
    }
  }

  ~Verse() {}

  const std::vector<RhymeId>& GetPossibleRhymes(bool period_does_follow,
                                                int same_rhyme_in_poem_count,
                                                const Model& model) {
    if (!initialized_)
      Initialize(model);

    const std::pair<bool, int> key =
        std::make_pair(period_does_follow, same_rhyme_in_poem_count);
    RhymeCache::iterator it = rhyme_cache_.find(key);
    if (it != rhyme_cache_.end())
      return it->second.second;

    std::pair<std::vector<std::vector<WordPosition> >,
              std::vector<RhymeId> >& vector_pair = rhyme_cache_[key];
    std::vector<std::vector<WordPosition> >& rhyme_map =
        vector_pair.first;
    std::vector<RhymeId>& result = vector_pair.second;
    const WordPosition period = WordPosition(kPeriod, num_syllables());
    if (period_does_follow) {
      Trellis::const_iterator it = reverse_trellis_.find(period);
      if (it != reverse_trellis_.end()) {
        rhyme_map.resize(model.num_rhymes());
        const std::vector<WordPosition>& predecessors = it->second;
        for (int i = 0; i < predecessors.size(); ++i) {
          const WordPosition wp = predecessors[i];
          rhyme_map.at(model.word(wp.word_id()).rhyme_id()).push_back(wp);
        }
      }
    } else {
      Trellis::const_iterator it = reverse_trellis_.upper_bound(period);
      if (it != reverse_trellis_.end()) {
        rhyme_map.resize(model.num_rhymes());
        for (/**/; it != reverse_trellis_.end(); ++it) {
          const WordPosition wp = it->first;
          rhyme_map.at(model.word(wp.word_id()).rhyme_id()).push_back(wp);
        }
      }
    }

    for (int i = 0; i < rhyme_map.size(); ++i) {
      if (!RhymesAreTrivial(rhyme_map[i], same_rhyme_in_poem_count, model)) {
        result.push_back(i);
      }
    }
    return result;
  }

  const void GetTrellisForRhyme(RhymeId rhyme_id, Trellis* trellis) {
    assert(initialized_);
    trellis->clear();
    // We take the first predecessor list at hand: the one for lines
    // that do not need to end with a period and need the least number
    // of distinct rhymes.
    RhymeCache::const_iterator it = rhyme_cache_.begin();
    assert(it != rhyme_cache_.end());
    const std::vector<WordPosition>& predecessors =
        it->second.first.at(rhyme_id);
    const WordPosition terminator =
        WordPosition(model_->root_id(), num_syllables());
    for (int i = 0; i < predecessors.size(); ++i) {
      (*trellis)[predecessors[i]].push_back(terminator);
    }

    Trellis::const_iterator end_interval = trellis->end();
    for (Position position = num_syllables(); position > kStartPosition;
         --position) {
      const Trellis::const_iterator begin_interval =
          trellis->lower_bound(WordPosition(kPeriod, position));
      assert(begin_interval != trellis->end());
      for (Trellis::const_iterator it = begin_interval; it != end_interval;
           ++it) {
        const WordPosition wp = it->first;
        assert(wp.position() == position);
        const Trellis::const_iterator source_it = reverse_trellis_.find(wp);
        assert(source_it != reverse_trellis_.end());
        const std::vector<WordPosition>& predecessors = source_it->second;
        for (int i = 0; i < predecessors.size(); ++i) {
          (*trellis)[predecessors[i]].push_back(wp);
        }
      }
      end_interval = begin_interval;
    }
  }

  Position num_syllables() const { return num_syllables_; }

 private:
  typedef std::map<WordPosition, std::vector<WordPosition>*> TrellisP;
  typedef std::map<std::pair<bool, int>,
                   std::pair<std::vector<std::vector<WordPosition> >,
                             std::vector<RhymeId> > > RhymeCache;

  void Initialize(const Model& model) {
    model_ = &model;
    std::vector<WordPosition> bootstrap;
    bootstrap.push_back(WordPosition(model.root_id(), kStartPosition));
    ExpandSuccessors(bootstrap);

    for (Position position = kStartPosition; position < num_syllables();
         ++position) {
      const WordPosition period = WordPosition(kPeriod, position);
      const WordPosition next_period = WordPosition(kPeriod, position + 1);
      for (TrellisP::const_iterator it = trellis_.upper_bound(period);
           it->first < next_period; ++it) {
        assert(it->first.position() == position);
        assert(it->second != NULL);
        ExpandSuccessors(*it->second);
      }
    }
    initialized_ = true;
  }

  void ExpandSuccessors(const std::vector<WordPosition>& successors) {
    for (int i = 0; i < successors.size(); ++i) {
      AddSuccessors(successors[i]);
    }
  }

  void AddSuccessors(WordPosition wp) {
    if (trellis_.find(wp) != trellis_.end())
      return;
    const WordId word_id = wp.word_id();
    const Position position = wp.position();
    std::vector<WordPosition>* expanded = new std::vector<WordPosition>;
    expanded->reserve(4);  // A heuristic speedup.
    const SuccessorMap& successor_map = model_->successor_map(word_id);
    const std::vector<Rhythm>& continuation = continuations_.at(position);
    for (int length = 1; length < continuation.size(); ++length) {
      const Rhythm rhythm = continuation[length];
      SuccessorMap::const_iterator it = successor_map.find(rhythm);
      if (it == successor_map.end())
        continue;
      const std::vector<WordId>& successor_vector = *it->second;
      const Position end_position = position + GetNumSyllables(rhythm);
      for (int i = 0; i < successor_vector.size(); ++i) {
        AddLink(wp, WordPosition(successor_vector[i], end_position), expanded);
      }
    }
    if (position == num_syllables() &&
        IsPeriodAmongSuccessors(successor_map)) {
      AddLink(wp, WordPosition(kPeriod, position), expanded);
    }
    trellis_.insert(make_pair(wp, expanded));
  }

  void AddLink(WordPosition wp, WordPosition next_wp,
               std::vector<WordPosition>* expanded) {
    expanded->push_back(next_wp);
    reverse_trellis_[next_wp].push_back(wp);
  }

  bool initialized_;
  Position num_syllables_;
  std::vector<Position> breaks_;
  std::vector<std::vector<Rhythm> > continuations_;
  TrellisP trellis_;
  Trellis reverse_trellis_;
  RhymeCache rhyme_cache_;
  const Model* model_;

  Verse(const Verse&);
  void operator=(const Verse&);
};

class Poem {
 public:
  Poem(const std::string& s, Random* random)
        : random_(random) {
    std::map<std::string, Verse*> created_verses;
    int last_break = 0;
    bool in_verse = false;
    for (int i = 0; i < s.size(); ++i) {
      if (s[i] == '.' || s[i] == '*' || s[i] == '?') {
        if (!in_verse) {
          formatting_.push_back(s.substr(last_break, i - last_break));
          last_break = i;
          in_verse = true;
        }
      } else if (s[i] == '|') {
        std::string line = s.substr(last_break, i - last_break);
        ++i;
        if (i >= s.size())
          Fatal("Brak zakończenia wiersza po '|'");
        bool period = false;
        if (s[i] == '|') {
          period = true;
          ++i;
          if (i >= s.size())
            Fatal("Brak zakończenia wiersza po '||'");
        }
        if (s[i] >= 'a' && s[i] <= 'z') {
          if (created_verses.find(line) == created_verses.end())
            created_verses[line] = new Verse(line);
          verses_.push_back(created_verses[line]);
          rhyme_occurrences_[s[i]].push_back(rhyme_chars_.size());
          rhyme_chars_.push_back(s[i]);
          period_does_follow_.push_back(period);
          last_break = i + 1;
          in_verse = false;
        }
      } else if (s[i] != ' ' && s[i] != '\n') {
        Fatal("Niespodziany znak '%c'", s[i]);
      }
    }
    period_does_follow_.back() = true;
    if (verses_.size() != formatting_.size())
      Fatal("Do czorta");
    assert(verses_.size() == rhyme_chars_.size());
    assert(verses_.size() == period_does_follow_.size());
  }

  ~Poem() {}

  void Init(const Model& model) {
    rhyme_ids_['\0'] = model.rhyme_id("");
    for (std::map<char, std::vector<int> >::const_iterator it =
         rhyme_occurrences_.begin(); it != rhyme_occurrences_.end(); ++it) {
      const char rhyme_char = it->first;
      const std::vector<int>& rhyme_verses = it->second;
      bool first_occurrence = true;
      std::vector<RhymeId> possible_rhymes;
      for (int i = 0; i < rhyme_verses.size(); ++i) {
        const std::vector<RhymeId>& new_rhymes =
            verses_[rhyme_verses[i]]->GetPossibleRhymes(
                period_does_follow_[i], rhyme_verses.size(), model);
        if (first_occurrence) {
          possible_rhymes = new_rhymes;
          first_occurrence = false;
        } else {
          std::vector<RhymeId> intersection;
          std::set_intersection(
              possible_rhymes.begin(), possible_rhymes.end(),
              new_rhymes.begin(), new_rhymes.end(),
              std::back_inserter(intersection));
          possible_rhymes.swap(intersection);
        }
      }
      if (possible_rhymes.empty())
        Fatal("Olaboga");
      allowed_rhymes_[rhyme_char] = possible_rhymes;
    }
    model_ = &model;
  }

  void GenerateRandomPoem(std::string* result) {
    std::vector<RhymeId> rhymes;
    do {
      verse_to_trellis_.clear();
      unexpandable_verses_.clear();
      poem_.clear();
      poem_.resize(1);
      backtracks_ = 0;
    } while (!GeneratePoemWithGivenRhymes(rhymes, result));
  }

  bool GeneratePoemWithGivenRhymes(const std::vector<RhymeId>& rhymes,
                                   std::string* result) {
    int i = 0;
    std::map<char, std::vector<RhymeId> >::const_iterator ita =
        allowed_rhymes_.begin();
    for (/**/; i < rhymes.size() && ita != allowed_rhymes_.end(); ++i, ++ita) {
      const char rhyme_char = ita->first;
      const std::vector<RhymeId>& rhyme_ids = ita->second;
      std::vector<RhymeId>::const_iterator itb = std::lower_bound(
          rhyme_ids.begin(), rhyme_ids.end(), rhymes[i]);
      if (itb == rhyme_ids.end())
        itb = rhyme_ids.begin();
      rhyme_ids_[rhyme_char] = *itb;
    }
    for (/**/; ita != allowed_rhymes_.end(); ++ita) {
      const char rhyme_char = ita->first;
      const std::vector<RhymeId>& allowed_rhyme_ids = ita->second;
      rhyme_ids_[rhyme_char] = FindDifferentRhymeId(allowed_rhyme_ids);
    }
    if (!AppendWordToPoem())
      return false;
    result->clear();
    ConvertPoemToString(result);
    return true;
  }

 private:
  bool BelongsToRhymeIds(RhymeId rhyme) {
    for (std::map<char, RhymeId>::const_iterator it = rhyme_ids_.begin();
         it != rhyme_ids_.end(); ++it) {
      if (it->second == rhyme)
        return true;
    }
    return false;
  }

  RhymeId FindDifferentRhymeId(const std::vector<RhymeId>& allowed_rhyme_ids) {
    const int index = random_->GetRandomIndex(allowed_rhyme_ids);
    for (int i = index; i < allowed_rhyme_ids.size(); ++i) {
      if (!BelongsToRhymeIds(allowed_rhyme_ids[i]))
        return allowed_rhyme_ids[i];
    }
    for (int i = index - 1; i >= 0; --i) {
      if (!BelongsToRhymeIds(allowed_rhyme_ids[i]))
        return allowed_rhyme_ids[i];
    }
    return allowed_rhyme_ids[index];
  }

  bool Backtrack() {
    ++backtracks_;
    return false;
  }

  bool RecentlyAddedRhymeIsTrivial() const {
    std::vector<WordPosition> rhymes;
    const int index = poem_.size() - 1;
    const char rhyme_char = rhyme_chars_[index];
    const std::map<char, std::vector<int> >::const_iterator it =
        rhyme_occurrences_.find(rhyme_char);
    assert(it != rhyme_occurrences_.end());
    const std::vector<int>& rhyme_verses = it->second;
    for (int i = 0; i < rhyme_verses.size() && rhyme_verses[i] <= index; ++i) {
      const std::vector<WordPosition>& verse = poem_[rhyme_verses[i]];
      assert(!verse.empty());
      if (verse.back().word_id() == kPeriod) {
        assert(verse.size() > 1);
        rhymes.push_back(verse[verse.size() - 2]);
      } else {
        rhymes.push_back(verse.back());
      }
    }
    return RhymesAreTrivial(rhymes, rhymes.size(), *model_);
  }

  bool TryOneWord(WordPosition wp, int num_syllables) {
    poem_.back().push_back(wp);
    if (wp.position() < num_syllables) {
      if (AppendWordToPoem())
        return true;
    } else if (!RecentlyAddedRhymeIsTrivial() &&
               !period_does_follow_[poem_.size() - 1]) {
      poem_.resize(poem_.size() + 1);
      if (AppendWordToPoem())
        return true;
      poem_.pop_back();
    }
    poem_.back().pop_back();
    return Backtrack();
  }

  bool TryOneWordWithPeriod(WordPosition wp, int num_syllables) {
    if (wp.position() < num_syllables)
      return false;
    poem_.back().push_back(wp);
    if (!RecentlyAddedRhymeIsTrivial()) {
      poem_.back().push_back(WordPosition(kPeriod, 0));
      poem_.resize(poem_.size() + 1);
      if (AppendWordToPoem())
        return true;
      poem_.pop_back();
      poem_.back().pop_back();
    }
    poem_.back().pop_back();
    return Backtrack();
  }

  bool TryVariousWords(const std::vector<WordPosition>& successor_vector) {
    if (successor_vector.empty())
      return Backtrack();
    assert(!poem_.empty());
    const int num_syllables = verses_[poem_.size() - 1]->num_syllables();
    int index = random_->GetRandomIndex(successor_vector);
    for (int i = index; i < successor_vector.size(); ++i) {
      if (TryOneWord(successor_vector[i], num_syllables))
        return true;
    }
    for (int i = index - 1; i >= 0; --i) {
      if (TryOneWord(successor_vector[i], num_syllables))
        return true;
    }
    if (TryOneWordWithPeriod(successor_vector[index], num_syllables))
      return true;
    return Backtrack();
  }

  bool AppendWordToPoem() {
    if (backtracks_ >= kMaxBacktrackCount)
      return false;
    const int index = poem_.size() - 1;
    if (index == verses_.size()) {
      poem_.pop_back();
      return true;
    }
    const char rhyme_char = rhyme_chars_.at(index);
    const RhymeId rhyme = rhyme_ids_[rhyme_char];
    std::pair<Verse*, char> key = std::make_pair(verses_.at(index), rhyme);
    if (verse_to_trellis_.find(key) == verse_to_trellis_.end()) {
      verses_[index]->GetTrellisForRhyme(rhyme, &verse_to_trellis_[key]);
    }
    const Trellis& trellis = verse_to_trellis_[key];

    if (poem_.back().empty()) {
      WordId previous_word;
      if (poem_.size() == 1) {
        previous_word = kPeriod;
      } else {
        previous_word = poem_[index - 1].back().word_id();
      }
      const std::pair<WordId, RhymeId> key =
          std::make_pair(previous_word, rhyme);
      if (unexpandable_verses_.find(key) != unexpandable_verses_.end())
        return false;
      const SuccessorMap& successor_map = model_->successor_map(previous_word);
      std::vector<WordPosition> successor_vector;
      for (SuccessorMap::const_iterator it = successor_map.begin();
           it != successor_map.end(); ++it) {
        const std::vector<WordId>* successor_part = it->second;
        for (int i = 0; i < successor_part->size(); ++i) {
          const WordId word_id = (*successor_part)[i];
          const WordPosition wp = WordPosition(
              word_id, GetNumSyllables(model_->word(word_id).rhythm()));
          if (trellis.find(wp) != trellis.end())
            successor_vector.push_back(wp);
        }
      }
      const bool result = TryVariousWords(successor_vector);
      if (!result)
        unexpandable_verses_.insert(key);
      return result;
    } else {
      WordPosition previous_wp = poem_.back().back();
      const Trellis::const_iterator it = trellis.find(previous_wp);
      assert(it != trellis.end());
      return TryVariousWords(it->second);
    }
  }

  void ConvertPoemToString(std::string* result) const {
    bool capitalize = true;
    int index = 0;
    int last_period = 0;
    for (int i = 0; i < poem_.size(); ++i) {
      result->append(formatting_[i]);
      for (int j = 0; j < poem_[i].size(); ++j) {
        if (poem_[i][j].word_id() == kPeriod) {
          if (VerseNeedsPeriod(i, index, last_period)) {
            result->append(".");
            capitalize = true;
            last_period = index;
          } else {
            result->append(",");
          }
        } else {
          if (!result->empty() &&
              result->at(result->size() - 1) != '\n' &&
              result->at(result->size() - 1) != ' ')
            result->append(" ");
          std::string word_form = model_->word(poem_[i][j].word_id()).form();
          if (capitalize)
            Capitalize(&word_form);
          result->append(word_form);
          capitalize = false;
        }
        ++index;
      }
    }
    result->append("\n");
  }

  bool EndsStanza(int i) const {
    if (i + 1 == poem_.size())
      return true;
    if (!formatting_[i + 1].empty() && formatting_[i + 1] != "\n")
      return true;
    return false;
  }

  bool VerseNeedsPeriod(int i, int index, int last_period) const {
    if (EndsStanza(i))
      return true;
    if (index - last_period <= kMinSentenceLength + 1)
      return false;
    assert(!poem_[i + 1].empty());
    if (EndsStanza(i + 1) && poem_[i + 1].size() <= kMinSentenceLength + 1)
      return false;
    return true;
  }

  static const int kMaxBacktrackCount = 15 * 1000;
  static const int kMinSentenceLength = 3;

  Random* random_;
  const Model* model_;
  std::vector<Verse*> verses_;
  std::vector<char> rhyme_chars_;
  std::vector<bool> period_does_follow_;
  std::vector<std::string> formatting_;
  std::map<char, std::vector<int> > rhyme_occurrences_;
  std::map<char, std::vector<RhymeId> > allowed_rhymes_;
  std::map<char, RhymeId> rhyme_ids_;
  std::map<std::pair<Verse*, RhymeId>, Trellis> verse_to_trellis_;
  std::set<std::pair<WordId, RhymeId> > unexpandable_verses_;
  std::vector<std::vector<WordPosition> > poem_;
  int backtracks_;

  Poem(const Poem&);
  void operator=(const Poem&);
};

int SkipWhitespace(const char* line) {
  int i = 0;
  for (/**/; line[i] != '\0'; ++i) {
    if (line[i] != ' ' && line[i] != '\n' && line[i] != '\t')
      return i;
  }
  return i;
}

void StripTrailingWhitespace(std::string* s) {
  while (!s->empty() &&
         (s->at(s->size() - 1) == ' ' ||
          s->at(s->size() - 1) == '\n' ||
          s->at(s->size() - 1) == '\t')) {
    s->resize(s->size() - 1);
  }
}

class Dispatcher {
 public:
  Dispatcher(Random* random) : random_(random) {}
  ~Dispatcher() {}

  void Init() {
    char line[1024];
    FILE* template_file = OpenFile(kTemplateFileName);
    std::string poem[2];
    bool in_poem = true;
    while (fgets(line, sizeof line, template_file) != NULL) {
      char* comment_start = strchr(line, '#');
      if (comment_start != NULL)
        *comment_start = '\0';
      const int fc = SkipWhitespace(line);
      if (line[fc] == '.' || line[fc] == '*' || line[fc] == '?') {
        in_poem = true;
      } else if (line[fc] != '\n' && line[fc] != '\0') {
        PushBackPoemIfNotEmpty(poem);
        in_poem = false;
      }
      poem[in_poem].append(line);
    }
    if (in_poem)
      PushBackPoemIfNotEmpty(poem);
    fclose(template_file);
  }

  void PrintPoem(const Model& model, int argc, char** argv) {
    std::string seed;
    std::string poem;
    int repetitions = 1;
    if (argc > 1 && argv[1][0] == '-') {
      int try_repetitions = atoi(&argv[1][1]);
      if (try_repetitions > 0) {
        repetitions = try_repetitions;
        --argc;
        ++argv;
      }
    }
    if (argc > 1) {
      random_->SetSeed(argv[1]);
    }
    seed = random_->GetSeed();
    int index = random_->GetRandomIndex(poem_generators_);
    poem_generators_[index]->Init(model);
    printf("Centon %s\n", seed.c_str());
    for (/**/; repetitions > 0; --repetitions) {
      poem_generators_[index]->GenerateRandomPoem(&poem);
      printf("\n%s", poem.c_str());
    }
    printf("\n");
  }

 private:
  void PushBackPoemIfNotEmpty(std::string poem[2]) {
    StripTrailingWhitespace(&poem[true]);
    if (!poem[true].empty()) {
      StripTrailingWhitespace(&poem[false]);
      poem_titles_.push_back(poem[false]);
      poem_generators_.push_back(new Poem(poem[true], random_));
      poem[false].clear();
      poem[true].clear();
    }
  }

  Random* random_;
  std::vector<std::string> poem_titles_;
  std::vector<Poem*> poem_generators_;

  Dispatcher(const Dispatcher&);
  void operator=(const Dispatcher&);
};

}  // namespace centonista

int main(int argc, char** argv) {
  using namespace centonista;
  Model model;
  model.Init();
  Random random;
  Dispatcher dispatcher(&random);
  dispatcher.Init();
  dispatcher.PrintPoem(model, argc, argv);
  return 0;
}
