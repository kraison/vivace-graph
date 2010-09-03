#!/usr/bin/perl

use LWP::UserAgent;
use URI::Escape;
 
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
    print $res->content . "\n";
  }
} else {
  print $res->status_line;
}
