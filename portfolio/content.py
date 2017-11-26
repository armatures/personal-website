"""Contentful access."""
from os import environ

from contentful import Client
from dotenv import load_dotenv, find_dotenv

load_dotenv(find_dotenv())

SPACE_ID = environ['CONTENTFUL_SPACE_ID']
ACCESS_TOKEN = environ['CONTENTFUL_ACCESS_TOKEN']

<<<<<<< HEAD
contentful = Client(SPACE_ID, ACCESS_TOKEN)


def get_entry(slug, content_type):
    """Retrieve content by slug and content_type."""
    entries = contentful.entries({
        'content_type': content_type,
        'fields.slug': slug,
    })
    assert len(entries) == 1
    return entries[0]


def get_method(slug):
    """Retrieve a method page."""
    return get_entry(slug, 'method')


def get_explainer(slug):
    """Retrieve an explainer page."""
    return get_entry(slug, 'explainer')


def get_landing(slug):
    """Retrieve a landing page."""
    return get_entry(slug, 'landingPage')
||||||| merged common ancestors
client = contentful.Client(SPACE_ID, ACCESS_TOKEN)
=======
contentful = Client(SPACE_ID, ACCESS_TOKEN)


def get_entries(content_type):
    """Get all entries for a content_type."""
    return contentful.entries({'content_type': content_type})


def get_entry(slug, content_type):
    """Retrieve content by slug and content_type."""
    entries = contentful.entries({
        'content_type': content_type,
        'fields.slug': slug,
    })
    assert len(entries) == 1
    return entries[0]


def get_method(slug):
    """Retrieve a method page."""
    return get_entry(slug, 'method')


def get_explainer(slug):
    """Retrieve an explainer page."""
    return get_entry(slug, 'explainer')


def get_explainers():
    """Retrieve all explainer pages."""
    return get_entries('explainer')


def get_landings():
    """Retrieve all landing pages."""
    return get_entries('landingPage')


def get_landing(slug):
    """Retrieve a landing page."""
    return get_entry(slug, 'landingPage')
>>>>>>> Add explainer pages
