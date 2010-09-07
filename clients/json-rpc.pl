#!/usr/bin/perl

use LWP::UserAgent;
use URI::Escape;
use JSON;
 
my $json = JSON->new->allow_nonref;
my $uri = 'http://localhost:9999/prolog/QUERY';
my $params = {
  "query" => "(select (?x ?y) (loves ?x ?y))",
  "graph" => 'test graph'
};

my $ua = new LWP::UserAgent;

my $res = $ua->post($uri,$params);
 
if($res){
  if ($res->is_error) {
    print "Error : ", $res->error_message;
  } else {
    my $d = $json->decode($res->content);
    foreach my $key (sort keys %{$d}) {
      if(ref($d->{$key}) eq 'ARRAY') {
	print "$key:\n";
        foreach (@{$d->{$key}}) {
          if(ref($_) eq 'ARRAY') {
            foreach  my $s (@{$_}) {
              print " $s ";
            }
            print "\n";
          } else {
            print " $_ ";
          }
	}
        print "\n";
      } else {
        print "$key: $d->{$key}\n";
      }
    }
  }
} else {
  print $res->status_line;
}
