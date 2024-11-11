import sys
import os
import requests
from readability import Document


def usage(argv):
    cmd = os.path.basename(argv[0])
    print('Usage: %s <url>\n'
          '(Example: "%s https://domain.com/article.html")' % (cmd, cmd))
    sys.exit(1)


if __name__ == '__main__':
    if len(sys.argv) != 2:
        usage(sys.argv)

    url = sys.argv[1]
    try:
        response = requests.get(url)
        if response.ok:
            doc = Document(response.text)
            print(doc.summary())
        else:
            print('Error fetching {}: {}'.format(url, response.reason))
    except:
        print('Error fetching {}: Connection Error'.format(url))
