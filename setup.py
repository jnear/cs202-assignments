from setuptools import setup

setup(name='cs202_support',
      version='0.1',
      description='Support code for CS202: compiler construction',
      url='http://github.com/jnear/cs202-assignments',
      author='Joe Near',
      author_email='jnear@uvm.edu',
      license='GPLv3',
      packages=['cs202_support'],
      install_requires=[
          'lark-parser',
      ],
      zip_safe=False)
