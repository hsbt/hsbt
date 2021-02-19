package main
import "github.com/awslabs/aws-sdk-go/aws"
import "github.com/awslabs/aws-sdk-go/gen/ec2"

creds := aws.Creds(accessKey, secretKey, "")
cli := ec2.New(creds, "us-west-2", nil)
resp, err := cli.DescribeInstances(nil)
if err != nil {
    panic(err)
}
fmt.Println(resp.Reservations)

