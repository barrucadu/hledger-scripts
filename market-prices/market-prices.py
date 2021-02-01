#!/usr/bin/env python3

from html.parser import HTMLParser
import json
import sys
import time
import urllib.request


def get_coinbase(base, currency):
    req = urllib.request.Request(
        "https://api.coinbase.com/v2/prices/{}-{}/spot/".format(base, currency),
        headers={"CB-VERSION": "2018-05-25"})
    with urllib.request.urlopen(req) as response:
        resp = json.load(response)
        return resp['data']['amount']


def ft_find_price(url, currency):
    class FTPriceFinder(HTMLParser):
        def __init__(self):
            HTMLParser.__init__(self)
            self.found = None
            self.isnext = False

        def handle_data(self, data):
            if data == 'Price ({})'.format(currency):
                self.isnext = True
            elif self.isnext:
                self.found = data
                self.isnext = False

    req = urllib.request.Request(url)
    try:
        with urllib.request.urlopen(req) as response:
            html = response.read().decode('utf-8')
            finder = FTPriceFinder()
            finder.feed(html)
            if finder.found is None:
                raise Exception("could not find price in {}".format(currency))
            else:
                return finder.found
    except Exception as e:
        raise Exception("{} on URL {}".format(e, url))


def get_ft_currency(base, currency):
    return ft_find_price(
        "https://markets.ft.com/data/currencies/tearsheet/summary?s={}{}".format(base, currency),
        currency)


def get_ft_security(stype, symbol, currency):
    return ft_find_price(
        "https://markets.ft.com/data/{}/tearsheet/summary?s={}".format(stype, symbol),
        currency)


ft_security_types = {
        "ft_equity": "equities",
        "ft_etf": "etfs",
        "ft_fund": "funds",
        "ft_index": "indices",
    }


config = json.load(sys.stdin)
symbols = config.get('symbols', {})
settings = config.get('settings', {})
settings.setdefault('default_currency', 'GBP')

for commodity, cconfig in config.get('commodities', {}).items():
    try:
        try:
            provider = cconfig['provider']
        except KeyError:
            raise Exception("missing provider")

        currency = cconfig.get('currency', settings['default_currency'])

        if provider == 'coinbase':
            rate = get_coinbase(
                cconfig.get('base', commodity),
                currency)
        elif provider == 'ft_currency':
            rate = get_ft_currency(
                cconfig.get('base', commodity),
                currency)
        elif provider in ft_security_types.keys():
            # backwards compatiblity
            isin = cconfig.get('isin')
            if provider == 'ft_fund' and isin:
                symbol = "{}:{}".format(isin, currency)
            else:
                symbol = cconfig.get('symbol', commodity)
            rate = get_ft_security(
                ft_security_types[provider],
                symbol,
                currency)
        else:
            raise Exception("unknown provider '{}'".format(provider))

        date = time.strftime('%Y-%m-%d')
        if currency in symbols:
            print('P {} {} {}{}'.format(date, commodity, symbols[currency], rate))
        else:
            print('P {} {} {} {}'.format(date, commodity, rate, currency))
    except Exception as e:
        print("; error processing commodity '{}': {}".format(commodity, e))
