#!/usr/bin/env python2.6
# -*- coding: utf-8 -*-

"""Preprocesses the resources of wolnelektury.pl for centonista."""

# Copyright (C) 2011  Marcin Ciura (mciura@gmail.com)
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as
# published by the Free Software Foundation, either version 3 of the
# License, or (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
#
# You should have received a copy of the GNU Affero General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

import locale
import re
import string
import StringIO
import sys
import urllib2
import zipfile

from nltk.tokenize import punkt


def Decode(s):
  return unicode(s, 'utf-8').encode('iso-8859-2')


def ForgivingDecode(s):
  return unicode(s, 'utf-8').encode('iso-8859-2', 'ignore')


def Encode(s):
  return unicode(s, 'iso-8859-2').encode('utf-8')


def CompileRegexp(s):
  return re.compile(Decode(s))


TXT_FILES = 'http://www.wolnelektury.pl/media/packs/txt-all.zip'

DEPUNCTUATE = re.compile('[][,;:/*"%&()<>_0-9]|--+| -|- ')
PERIODIZE = re.compile('[?!]')
DEPERIODIZE = re.compile('[.](?=.)')

OPT_CONSONANTS = '[bcćçčdfghjklłmnńpqrsśštvwxzźžż]*'
OPT_NONSYLLABIC = (
    '(?:(?:(?<=l))y(?:(?=o))|(?:(?<=g))u(?:(?=[eiy]))|(?:(?<=q)u))?')
MONOSYLLABIC = (
    'ae$|[aeo]y|(?:(?<!arc))y[ao]|au|eau|eu|ée|oeh|ou|'
    '(?:(?<=[dn]))ai(?:(?=s))|(?:(?<=[lw]))ai|'
    '(?:(?<=[^i]m))ai(?:(?=[lns]))|(?:(?<=[ln]t))ai|'
    '(?:(?<=[blz]))ei(?:(?=t))|(?:(?<=[mw]))ei(?:(?=n))|'
    '(?:(?<=st))ei(?:(?=n))|ei(?:(?=f))|'
    '(?:(?<=v))oi|(?:(?<=çen))oi(?:(?=s))|oi(?:(?=x))')
VOWEL = '[aąáâäeéęëiíîoóôöuúüyý]'

SUBSTITUTIONS = [
    (CompileRegexp('austria([ck])'), 'austryja\\1'),
    (CompileRegexp('^hm'), 'hym'),
    (CompileRegexp('klien([ct])'), 'klijen\\1'),
    (CompileRegexp('marz([lłn])'), 'mars\\1'),
    (CompileRegexp('^(m[iu]r)z([aąęoó]|y$|y[^nń])'), '\\1s\\2'),
    (CompileRegexp('patrio([ct])'), 'patryjo\\1'),
    (CompileRegexp('^sir$'), 'ser'),
    (CompileRegexp('żmii$'), 'żmiji'),
    (CompileRegexp('^(a+|e+|i+|o+|u+|y+)$'), lambda m: m.group(1)[0]),
]

SYLLABLE = CompileRegexp(
    "(?:e'|" + OPT_CONSONANTS + OPT_NONSYLLABIC + 'i?)?'
    '(?:' + MONOSYLLABIC + '|' + VOWEL + ')'
    '(?:' + OPT_CONSONANTS + OPT_NONSYLLABIC + ')')

MORE_SYLLABLES = CompileRegexp(
    'auł|nau(?:b[il]|cz|ga|j|k|m(?:(?!ach))|r[ąz]|w|ż)|'
    'prau|zau(?:f|ł|r[ao].|s[tz]|w)|'
    '(?:ant|w|rz)y[ao]|'
    'eusz|nieu|przeu|seul|'
    '[dnpw]ou|długou|samou')

INITIAL_CONSONANTS = CompileRegexp(
    '^' + OPT_CONSONANTS + OPT_NONSYLLABIC +
    '(?:i?(?=' + VOWEL + '))')

INITIAL_VOWEL = CompileRegexp(
    '^' + VOWEL)

ULTIMATE = CompileRegexp(
    '[âçîô]|eau|eu$|(?<!wi)é$|([^ir]|[^abks]r)é[^ćjmr]|'
    '[dn]ais|[lw]ai|[^is]mai[lns]|[ln]tai|voi|[çen]ois|oix|eui')

ANTEPENULTIMATE = CompileRegexp(
    '(ł[ao]?|^byle|^chociaż|^jeśli|^jeżeli)(bym|byś|by)|'
    '(li|ły)(by|śmy|ście)$|'
    '(([afgm]|([by]|ta|cyk|ato|ncho)l|([hi]|[hmp]a|[lmrt]o)n|[eip]p)i|'
    '(([els]|[lt]o)d|ir|as|([eknpt]|[mn]a|li|[rz]o|.s|[ae]u|ry)t|'
    '(la|ab|e|f|[ft]o|p|met)r|[iuy]z)y)'
    '(ka|kiem|ku|cy|kach|kom|ce|ką|kę|ki|ko)$|'
    '^cztery.|[^lrs]set$|kroć$|imum$|bruderszaf|cyferbla|rzecz.*pospolit|'
    '(^a|^aże|^choć|^gdy|^jak|^że)(byśmy|byście)')

PREANTEPENULTIMATE = CompileRegexp(
    '(li|ły|^byle|^chociaż|^jeśli|^jeżeli)(byśmy|byście)$')

PROCLITICS = dict.fromkeys(Decode("""
    bez beze dla do ku na nad nade o od ode po pod pode przed przede
    przez przeze przy spod spode u w we wśród z ze za znad znade zza
    nie nié
""").split(), '_')

PROCLITICS.update(dict.fromkeys(Decode("""
    a aż bo choć co czy gdy gdyż i iż jak lecz lub niż zaś że żem żeś
    le la les de du del des al au the of der die das
""").split(), '~'))

ENCLITICS = set(Decode("""
    em eś li no się se to
    mię mnie mi mną ci cię mu go nim jej ją nią
    my nas nam wy was wam ich im
""").split())

SPURIOUS_WORDS = CompileRegexp(
    '(itd|(.-)+.|C{,4}X?[LC]?X{,4}I?[VX]?I{,4})$')

# NOTE: ę$:e belongs to FINAL_RHYME_RULES while ą$:o belongs to
# GENERIC_RHYME_RULES. Thanks to this, "chcę go" rhymes with "ego"
# while "chcą go" rhymes with "Kongo".
FINAL_RHYME_RULES = [
    (CompileRegexp(pr.split(':')[0]), Decode(pr.split(':')[1])) for pr in """
    tz$:c trz$:cz (?<!o)ck$:k chs$:ks cks$:ks stw$:s dt$:t th$:t
    ff$:f ll$:l łł$:ł ss$:s tt$:t
    bł$:b chł$:ch dł$:d gł$:g kł$:k pł$:p rł$:r sł$:s tł$:t zł$:z
    n$:m
    ę$:e
""".split()]

# TODO: po wielu spółgłoskach -ii można przerobić na -i.
# TODO: po wielu spółgłoskach -i[aeou] należy przerobić na -j[aeou].
GENERIC_RHYME_RULES = [
    (CompileRegexp(pr.split(':')[0]), Decode(pr.split(':')[1])) for pr in """
    dz$:c dż$:cz dź$:ć w$:f g$:k b$:p (?<![crs])z$:s rz$:sz ż$:sz ższ$:sz
    strz$:szcz zdrz$:szcz żdż$:szcz ź$:ś źć$:ść źdź$:ść d$:t
    iej$:i (?<=[jl])ej$:i (?<![ijl])ej$:y
    (<=[aąáâäeéęëoóôöuúüyý])i:ji

    ^i:y ch:h (?<=[^hkpt])rz:ż (?<=[hkpt])rz:sz ó:u

    b(?=[cćfhkpsśt]):p             p(?=[bdgźż]):b
    d(?=[cćfhkpsśt]):t             t(?=[bdgźż]):d
    dz(?=[cćfhkpsśt]):c            c(?=[bdgźż]):dz
    dź(?=[cćfhkpsśt]):ć            ć(?=[bdgźż]):dź
    dż(?=[cćfhkpsśt]):cz           cz(?=[bdgźż]):dż
    g(?=[cćfhkpsśt]):k             k(?=[bdgźż]):g
    w(?=[cćfhkpsśt]):f             f(?=[bdgźż]):w
    (?<![cdrs])z(?=[cćfhkpsśt]):s  s(?=[bdgźż]):z
    (?<!d)ź(?=[cćfhkpsśt]):ś       ś(?=[bdgźż]):ź
    ((?<!d)ż)(?=[cćfhkpsśt]):sz    sz(?=[bdgźż]):ż

    (?<=[śź])l(?=[cmn]): błk:pk wsk:sk
    ight:ajt ais$:e eaux?:o ault:o au(?!(cz|k|ł)):ał
    ohm:om ohn:on ou(?!ch|st):u v:w x:ks tsch:cz

    ą(?=[ćfhsśwzźż]):oł ą(?=[bp]):om ą(?=[cdgkt]):on ą(?=[lł]):o ą$:o
    ę(?=[ćfhsśwzźż]):eł ę(?=[bp]):em ę(?=[cdgkt]):en ę(?=[lł]):e
""".split()]

ALPHABET = (
    string.digits + string.ascii_uppercase + string.ascii_lowercase + '+/')


def GetRhythmAndRhyme2(word):
  nword = word
  for pattern, replacement in SUBSTITUTIONS:
    nword = pattern.sub(replacement, nword)
  syllables = SYLLABLE.findall(nword)
  coda = [''.join(syllables[-i:]) for i in xrange(5)]
  length = len(syllables)
  m = MORE_SYLLABLES.search(nword)
  if m:
    for i in xrange(len(coda)):
      if m.end() > len(word) - len(coda[i]) and length > 1:
        coda[i] = INITIAL_CONSONANTS.sub('', coda[i])
        coda[i] = INITIAL_VOWEL.sub('', coda[i])
    # TODO: increment length by the number of occurrences of MORE_SYLLABLES.
    length += 1
  rhythm = ['.'] * length
  if word in PROCLITICS or word in ENCLITICS or length == 0:
    accent = 0
  elif ULTIMATE.search(word) or length == 1:
    accent = 1
  elif ANTEPENULTIMATE.search(word) and length >= 3:
    accent = 3
  elif PREANTEPENULTIMATE.search(word) and length >= 4:
    accent = 4
  else:
    accent = 2
  if accent:
    rhythm[-accent] = '*'
  else:
    accent = 1
  rhyme = INITIAL_CONSONANTS.sub('', coda[accent])
  for pattern, replacement in FINAL_RHYME_RULES:
    rhyme = pattern.sub(replacement, rhyme)
  return ''.join(rhythm), rhyme


def GetRhythmAndRhyme1(word):
  chunks = word.split('-')
  result = [GetRhythmAndRhyme2(x) for x in chunks]
  rhythm = ''.join(x for x, y in result)
  if len(chunks) == 1 or result[-1][1]:
    return rhythm, result[-1][1]
  else:
    return rhythm, GetRhythmAndRhyme2(''.join(chunks))[1]


def GetRhythmAndRhyme(word):
  chunks = word.lower().split('~')
  result = [GetRhythmAndRhyme1(chunk) for chunk in chunks]
  rhythm = ''.join(x for x, y in result)
  tail = []
  for part, chunk in reversed(zip(result, chunks)):
    if '*' in part[0]:
      rhyme = part[1] + ''.join(reversed(tail))
      break
    tail.append(chunk)
  else:
    if rhythm:
      rhythm = rhythm[:-1] + '*'
    rhyme = result[-1][1]
  if tail:
    for pattern, replacement in FINAL_RHYME_RULES:
      rhyme = pattern.sub(replacement, rhyme)
  for pattern, replacement in GENERIC_RHYME_RULES:
    rhyme = pattern.sub(replacement, rhyme)
  return rhythm, rhyme


def CleanUpLine(s):
  s = s.replace('\n', ' ')
  s = DEPUNCTUATE.sub(' ', s)
  s = PERIODIZE.sub('.', s)
  s = DEPERIODIZE.sub('', s)
  s = s.replace('.', ' .')
  s = s.strip()
  return s


def GetChunks(input_text):
  last = '.'
  proclitic_link = None
  for word in input_text.split():
    if not proclitic_link or word == '.':
      if word in ENCLITICS and last != '.':
        last += '~' + word
      else:
        yield last
        last = word
    else:
      last += proclitic_link + word
    proclitic_link = PROCLITICS.get(word, None)
  yield last
  yield '.'


def Base32(n):
  result = []
  while True:
    n, r = divmod(n, 32)
    result.append(r)
    if n == 0:
      result[0] += 32
      return ''.join(ALPHABET[r] for r in reversed(result))


def EncodeRhythm(rhythm):
  result = (1 << len(rhythm)) - 1
  for position, accent in enumerate(rhythm):
    if accent == '*':
      result += 1 << position
  return result


def main():
  locale.setlocale(locale.LC_ALL, 'pl_PL.ISO8859-2')
  if len(sys.argv) == 1:
    pack = TXT_FILES
  elif sys.argv[1] != '-':
    pack = sys.argv[1]
  else:
    for word in GetChunks(ForgivingDecode(sys.stdin.read())):
      rhythm, rhyme = GetRhythmAndRhyme(word)
      print '%s [%s] [%s]' % (Encode(word), rhythm, Encode(rhyme))
    sys.exit()

  sys.stderr.write('Reading pack\n')
  input_texts = []
  txt_zip = zipfile.ZipFile(StringIO.StringIO(urllib2.urlopen(pack).read()))
  for filename in txt_zip.namelist():
    input_texts.append(txt_zip.read(filename).split('\n-----\n')[0])
  del txt_zip

  sys.stderr.write('Training tokenizer\n')
  trainer = punkt.PunktTrainer()
  trainer.INCLUDE_ALL_COLLOCS = True
  text = ForgivingDecode('\n'.join(input_texts))
  trainer.train(text)
  del input_texts

  sys.stderr.write('Tokenizing text\n')
  sbd = punkt.PunktSentenceTokenizer(trainer.get_params())
  lines = []
  for line in sbd.sentences_from_text(text, realign_boundaries=True):
    lines.append(CleanUpLine(line))
  del text
  del trainer

  sys.stderr.write('Creating model\n')
  model = {}
  case_aware_model = {}
  case_variants = {}
  previous_word = '.'
  lowercase_previous = '.'
  for next_word in GetChunks('\n'.join(lines)):
    if SPURIOUS_WORDS.match(next_word):
      continue
    lowercase_next = next_word.lower()
    if lowercase_next in PROCLITICS or lowercase_next in ENCLITICS:
      continue
    if previous_word == '.':
      next_word = lowercase_next
    if lowercase_previous in model:
      model[lowercase_previous].add(lowercase_next)
    else:
      model[lowercase_previous] = set([lowercase_next])
    if lowercase_previous not in case_variants:
      case_variants[lowercase_previous] = {}
    variants = case_variants[lowercase_previous]
    if previous_word in variants:
      variants[previous_word] += 1
    else:
      variants[previous_word] = 1
    previous_word = next_word
    lowercase_previous = lowercase_next

  for word, variants in case_variants.iteritems():
    flipped_variants = [(count, variant)
                        for variant, count in variants.iteritems()]
    flipped_variants.sort()
    # Tie breaking favors lowercase.
    most_frequent_variant = flipped_variants[-1][1]
    case_aware_model[most_frequent_variant] = model[word]

  array = case_aware_model.items()
  array.sort(key=lambda x: len(x[1]), reverse=True)
  if array[0][0] != '.':
    sys.exit('Olaboga, [%s]!' % array[0][0])
  array[0][1].discard('.')
  index = dict((word.lower(), position)
               for position, (word, _) in enumerate(array))
  del lines
  del model
  del case_aware_model
  del case_variants

  sys.stderr.write('Saving model.d\n')
  model_file = open('model.d', 'w')
  for _, successors in array:
    numbers = [index[x] for x in successors]
    numbers.sort()
    previous_number = 0
    for number in numbers:
      model_file.write(Base32(number - previous_number))
      previous_number = number
    model_file.write('\n')
  model_file.close()
  del index

  sys.stderr.write('Computing metric\n')
  metric = []
  rhymes = {}
  for word, _ in array:
    rhythm, rhyme = GetRhythmAndRhyme(word)
    metric.append((rhythm, rhyme))
    if rhyme in rhymes:
      rhymes[rhyme] += 1
    else:
      rhymes[rhyme] = 1
  rhyme_array = rhymes.items()
  rhyme_array.sort(key=lambda x: x[1], reverse=True)
  rhyme_index = dict((rhyme, position)
                     for position, (rhyme, _) in enumerate(rhyme_array))
  del rhymes

  sys.stderr.write('Saving rymy.d\n')
  rhyme_file = open('rymy.d', 'w')
  for rhyme, _ in rhyme_array:
    rhyme_file.write('%s\n' % Encode(rhyme))
  rhyme_file.close()
  del rhyme_array

  sys.stderr.write('Saving zestroje.d\n')
  word_file = open('zestroje.d', 'w')
  for i in xrange(len(array)):
    word_file.write('%s %s%s\n' %
                    (Encode(array[i][0].replace('~', '_')),
                     Base32(EncodeRhythm(metric[i][0])),
                     Base32(rhyme_index[metric[i][1]])))
  word_file.close()
  sys.stderr.write('Done\n')
  del array
  del metric


if __name__ == '__main__':
  main()
