#import <Foundation/Foundation.h>

// clang small-class.m -framework Foundation
// ./a.out

// https://stackoverflow.com/a/7987150/22147
// https://stackoverflow.com/a/6203592/22147

@interface SmallClass: NSObject {

}

- (int)get1977;

@end

@implementation SmallClass

- (instancetype)init {
	self = [super init];

	return self;

}

- (int)get1977 {
	return 1977;
}


@end


int
main(int argc, char **argv) {
	SmallClass *s = [[SmallClass alloc] init];

	NSLog(@"get 1977: %i", [s get1977]);

	return 0;
}
