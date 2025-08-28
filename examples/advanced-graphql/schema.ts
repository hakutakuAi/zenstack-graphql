import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType, InterfaceType } from "type-graphql";
import { GraphQLJSON } from "graphql-scalars";
import "reflect-metadata";

export enum ProductStatus {
    DRAFT = "DRAFT",
    PUBLISHED = "PUBLISHED",
    ARCHIVED = "ARCHIVED"
}

registerEnumType(ProductStatus, {
  name: 'ProductStatus',
})

export enum ReviewRating {
    ONE = "ONE",
    TWO = "TWO",
    THREE = "THREE",
    FOUR = "FOUR",
    FIVE = "FIVE"
}

registerEnumType(ReviewRating, {
  name: 'ReviewRating',
})

@ObjectType({ description: "Product in the e-commerce system" })
export class Product {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => Date)
    updatedAt!: Date;
    @Field(() => String)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => Float)
    price!: number;
    @Field(() => ProductStatus)
    status!: ProductStatus;
    @Field(() => GraphQLJSON, { nullable: true })
    metadata?: any;
    @Field(() => [Review])
    reviews!: Review[];
    @Field(() => [ProductTag])
    tags!: ProductTag[];
}

@ObjectType({ description: "Customer review for a product" })
export class Review {
    @Field(() => String)
    id!: string;
    @Field(() => Date)
    createdAt!: Date;
    @Field(() => String)
    title!: string;
    @Field(() => String)
    content!: string;
    @Field(() => ReviewRating)
    rating!: ReviewRating;
    @Field(() => Boolean)
    verified!: boolean;
    @Field(() => Int)
    helpfulCount!: number;
    @Field(() => Product)
    product!: Product;
    @Field(() => String)
    productId!: string;
}

@ObjectType({ description: "Tag for categorizing products" })
export class Tag {
    @Field(() => String)
    id!: string;
    @Field(() => String)
    name!: string;
    @Field(() => String, { nullable: true })
    color?: string;
    @Field(() => [ProductTag])
    products!: ProductTag[];
}

@ObjectType({ description: "Many-to-many relation between Product and Tag" })
export class ProductTag {
    @Field(() => Product)
    product!: Product;
    @Field(() => String)
    productId!: string;
    @Field(() => Tag)
    tag!: Tag;
    @Field(() => String)
    tagId!: string;
    @Field(() => Date)
    assignedAt!: Date;
}

@InputType()
export class ForwardPaginationInput {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
}

@InputType()
export class BackwardPaginationInput {
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class PaginationInput {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@ObjectType()
export class PageInfo {
    @Field(() => Boolean)
    hasNextPage!: boolean;
    @Field(() => Boolean)
    hasPreviousPage!: boolean;
    @Field(() => String, { nullable: true })
    startCursor?: string | undefined;
    @Field(() => String, { nullable: true })
    endCursor?: string | undefined;
}

@InterfaceType({ description: 'Base interface for all edge types in connections', autoRegisterImplementations: false })
export abstract class Edge {
    @Field(() => String, { description: 'A cursor for use in pagination' })
    cursor!: string;
}

@InterfaceType({ description: 'Base interface for all connection types', autoRegisterImplementations: false })
export abstract class Connection {
    @Field(() => PageInfo, { description: 'Information to aid in pagination' })
    pageInfo!: PageInfo;
    @Field(() => Int, { description: 'The total count of items in the connection' })
    totalCount!: number;
}

@ObjectType({ implements: Edge })
export class ProductEdge implements Edge {
    @Field(() => Product)
    node!: Product;
    @Field(() => String)
    cursor!: string;
}

@ObjectType({ implements: Connection })
export class ProductConnection implements Connection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [ProductEdge])
    edges!: ProductEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType({ implements: Edge })
export class ReviewEdge implements Edge {
    @Field(() => Review)
    node!: Review;
    @Field(() => String)
    cursor!: string;
}

@ObjectType({ implements: Connection })
export class ReviewConnection implements Connection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [ReviewEdge])
    edges!: ReviewEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType({ implements: Edge })
export class TagEdge implements Edge {
    @Field(() => Tag)
    node!: Tag;
    @Field(() => String)
    cursor!: string;
}

@ObjectType({ implements: Connection })
export class TagConnection implements Connection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [TagEdge])
    edges!: TagEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType({ implements: Edge })
export class ProductTagEdge implements Edge {
    @Field(() => ProductTag)
    node!: ProductTag;
    @Field(() => String)
    cursor!: string;
}

@ObjectType({ implements: Connection })
export class ProductTagConnection implements Connection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [ProductTagEdge])
    edges!: ProductTagEdge[];
    @Field(() => Int)
    totalCount!: number;
}

export enum SortDirection {
    ASC = "ASC",
    DESC = "DESC"
}

registerEnumType(SortDirection, {
  name: 'SortDirection',
  description: 'Sort direction for ordering results',
})

@InputType()
export class ProductSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    updatedAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    name?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    price?: SortDirection | undefined;
}

@InputType()
export class ReviewSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    helpfulCount?: SortDirection | undefined;
}

@InputType()
export class TagSortInput {
    @Field(() => SortDirection, { nullable: true })
    _placeholder?: SortDirection | undefined;
}

@InputType()
export class ProductTagSortInput {
    @Field(() => SortDirection, { nullable: true })
    _placeholder?: SortDirection | undefined;
}

@InputType()
export class NumericFilterInput {
    @Field(() => Float, { nullable: true })
    equals?: number | undefined;
    @Field(() => Float, { nullable: true })
    not?: number | undefined;
    @Field(() => Float, { nullable: true })
    gt?: number | undefined;
    @Field(() => Float, { nullable: true })
    gte?: number | undefined;
    @Field(() => Float, { nullable: true })
    lt?: number | undefined;
    @Field(() => Float, { nullable: true })
    lte?: number | undefined;
}

@InputType()
export class StringFilterInput {
    @Field(() => String, { nullable: true })
    equals?: string | undefined;
    @Field(() => String, { nullable: true })
    not?: string | undefined;
    @Field(() => [String!], { nullable: true })
    in?: string[] | undefined;
    @Field(() => [String!], { nullable: true })
    notIn?: string[] | undefined;
    @Field(() => String, { nullable: true })
    contains?: string | undefined;
    @Field(() => String, { nullable: true })
    startsWith?: string | undefined;
    @Field(() => String, { nullable: true })
    endsWith?: string | undefined;
}

@InputType()
export class BooleanFilterInput {
    @Field(() => Boolean, { nullable: true })
    equals?: boolean | undefined;
    @Field(() => Boolean, { nullable: true })
    not?: boolean | undefined;
}

@InputType()
export class DateTimeFilterInput {
    @Field(() => Date, { nullable: true })
    equals?: Date | undefined;
    @Field(() => Date, { nullable: true })
    not?: Date | undefined;
    @Field(() => Date, { nullable: true })
    gt?: Date | undefined;
    @Field(() => Date, { nullable: true })
    gte?: Date | undefined;
    @Field(() => Date, { nullable: true })
    lt?: Date | undefined;
    @Field(() => Date, { nullable: true })
    lte?: Date | undefined;
}

@InputType()
export class ProductStatusFilterInput {
    @Field(() => ProductStatus, { nullable: true })
    equals?: ProductStatus | undefined;
    @Field(() => ProductStatus, { nullable: true })
    not?: ProductStatus | undefined;
    @Field(() => [ProductStatus!], { nullable: true })
    in?: ProductStatus[] | undefined;
    @Field(() => [ProductStatus!], { nullable: true })
    notIn?: ProductStatus[] | undefined;
    @Field(() => [ProductStatusFilterInput!], { nullable: true })
    AND?: ProductStatusFilterInput[] | undefined;
    @Field(() => [ProductStatusFilterInput!], { nullable: true })
    OR?: ProductStatusFilterInput[] | undefined;
}

@InputType()
export class ReviewRatingFilterInput {
    @Field(() => ReviewRating, { nullable: true })
    equals?: ReviewRating | undefined;
    @Field(() => ReviewRating, { nullable: true })
    not?: ReviewRating | undefined;
    @Field(() => [ReviewRating!], { nullable: true })
    in?: ReviewRating[] | undefined;
    @Field(() => [ReviewRating!], { nullable: true })
    notIn?: ReviewRating[] | undefined;
    @Field(() => [ReviewRatingFilterInput!], { nullable: true })
    AND?: ReviewRatingFilterInput[] | undefined;
    @Field(() => [ReviewRatingFilterInput!], { nullable: true })
    OR?: ReviewRatingFilterInput[] | undefined;
}

@InputType()
export class ProductFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => NumericFilterInput, { nullable: true })
    price?: NumericFilterInput | undefined;
    @Field(() => ProductStatusFilterInput, { nullable: true })
    status?: ProductStatusFilterInput | undefined;
    @Field(() => [ProductFilterInput!], { nullable: true })
    AND?: ProductFilterInput[] | undefined;
    @Field(() => [ProductFilterInput!], { nullable: true })
    OR?: ProductFilterInput[] | undefined;
}

@InputType()
export class ReviewFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    title?: StringFilterInput | undefined;
    @Field(() => ReviewRatingFilterInput, { nullable: true })
    rating?: ReviewRatingFilterInput | undefined;
    @Field(() => BooleanFilterInput, { nullable: true })
    verified?: BooleanFilterInput | undefined;
    @Field(() => [ReviewFilterInput!], { nullable: true })
    AND?: ReviewFilterInput[] | undefined;
    @Field(() => [ReviewFilterInput!], { nullable: true })
    OR?: ReviewFilterInput[] | undefined;
}

@InputType()
export class TagFilterInput {
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => [TagFilterInput!], { nullable: true })
    AND?: TagFilterInput[] | undefined;
    @Field(() => [TagFilterInput!], { nullable: true })
    OR?: TagFilterInput[] | undefined;
}

@InputType()
export class ProductTagFilterInput {
    @Field(() => [ProductTagFilterInput!], { nullable: true })
    AND?: ProductTagFilterInput[] | undefined;
    @Field(() => [ProductTagFilterInput!], { nullable: true })
    OR?: ProductTagFilterInput[] | undefined;
}

@InputType({ description: "Create input for Product" })
export class ProductCreateInput {
    @Field(() => String!)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => Float!)
    price!: number;
    @Field(() => ProductStatus!)
    status!: ProductStatus;
    @Field(() => GraphQLJSON, { nullable: true })
    metadata?: any;
}

@InputType({ description: "Update input for Product" })
export class ProductUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => Date)
    createdAt?: Date;
    @Field(() => Date)
    updatedAt?: Date;
    @Field(() => String)
    name?: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => Float)
    price?: number;
    @Field(() => ProductStatus)
    status?: ProductStatus;
    @Field(() => GraphQLJSON, { nullable: true })
    metadata?: any;
}

@InputType({ description: "Create input for Review" })
export class ReviewCreateInput {
    @Field(() => String!)
    title!: string;
    @Field(() => String!)
    content!: string;
    @Field(() => ReviewRating!)
    rating!: ReviewRating;
    @Field(() => Boolean!)
    verified!: boolean;
    @Field(() => Int!)
    helpfulCount!: number;
    @Field(() => String!)
    productId!: string;
}

@InputType({ description: "Update input for Review" })
export class ReviewUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => Date)
    createdAt?: Date;
    @Field(() => String)
    title?: string;
    @Field(() => String)
    content?: string;
    @Field(() => ReviewRating)
    rating?: ReviewRating;
    @Field(() => Boolean)
    verified?: boolean;
    @Field(() => Int)
    helpfulCount?: number;
    @Field(() => String)
    productId?: string;
}

@InputType({ description: "Create input for Tag" })
export class TagCreateInput {
    @Field(() => String!)
    name!: string;
    @Field(() => String, { nullable: true })
    color?: string;
}

@InputType({ description: "Update input for Tag" })
export class TagUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => String)
    name?: string;
    @Field(() => String, { nullable: true })
    color?: string;
}

@InputType({ description: "Create input for ProductTag" })
export class ProductTagCreateInput {
    @Field(() => String!)
    productId!: string;
    @Field(() => String!)
    tagId!: string;
    @Field(() => Date!)
    assignedAt!: Date;
}

@InputType({ description: "Update input for ProductTag" })
export class ProductTagUpdateInput {
    @Field(() => String)
    productId?: string;
    @Field(() => String)
    tagId?: string;
    @Field(() => Date)
    assignedAt?: Date;
}

@InputType()
export class ProductQueryArgs {
    @Field(() => ProductFilterInput, { nullable: true })
    filter?: ProductFilterInput | undefined;
    @Field(() => ProductSortInput, { nullable: true })
    sort?: ProductSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class ReviewQueryArgs {
    @Field(() => ReviewFilterInput, { nullable: true })
    filter?: ReviewFilterInput | undefined;
    @Field(() => ReviewSortInput, { nullable: true })
    sort?: ReviewSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class TagQueryArgs {
    @Field(() => TagFilterInput, { nullable: true })
    filter?: TagFilterInput | undefined;
    @Field(() => TagSortInput, { nullable: true })
    sort?: TagSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}

@InputType()
export class ProductTagQueryArgs {
    @Field(() => ProductTagFilterInput, { nullable: true })
    filter?: ProductTagFilterInput | undefined;
    @Field(() => ProductTagSortInput, { nullable: true })
    sort?: ProductTagSortInput | undefined;
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
}
