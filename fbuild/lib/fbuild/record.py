class Record(dict):
    def __setattr__(self, key, value):
        'Allow setting items as if they were attributes.'

        self[key] = value

    def __getattr__(self, key):
        'Allow items to be exposed as attributes.'

        try:
            return self[key]
        except KeyError as e:
            raise AttributeError(e) from e
