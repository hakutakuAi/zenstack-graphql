import { ObjectType, Field, ID, Int, Float, registerEnumType, InputType, ArgsType } from "type-graphql";
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
    @Field(() => JSON, { nullable: true })
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

@ObjectType()
export class ProductEdge {
    @Field(() => Product)
    node!: Product;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class ProductConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [ProductEdge])
    edges!: ProductEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class ReviewEdge {
    @Field(() => Review)
    node!: Review;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class ReviewConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [ReviewEdge])
    edges!: ReviewEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class TagEdge {
    @Field(() => Tag)
    node!: Tag;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class TagConnection {
    @Field(() => PageInfo)
    pageInfo!: PageInfo;
    @Field(() => [TagEdge])
    edges!: TagEdge[];
    @Field(() => Int)
    totalCount!: number;
}

@ObjectType()
export class ProductTagEdge {
    @Field(() => ProductTag)
    node!: ProductTag;
    @Field(() => String)
    cursor!: string;
}

@ObjectType()
export class ProductTagConnection {
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
export class ProductSortInputSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    updatedAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    price?: SortDirection | undefined;
}

@InputType()
export class ReviewSortInputSortInput {
    @Field(() => SortDirection, { nullable: true })
    createdAt?: SortDirection | undefined;
    @Field(() => SortDirection, { nullable: true })
    helpfulCount?: SortDirection | undefined;
}

@InputType()
export class TagSortInputSortInput {
    @Field(() => SortDirection, { nullable: true })
    _placeholder?: SortDirection | undefined;
}

@InputType()
export class ProductTagSortInputSortInput {
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
    lt?: number | undefined;
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
    lt?: Date | undefined;
}

@InputType()
export class ProductFilterInputFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => NumericFilterInput, { nullable: true })
    price?: NumericFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    status?: StringFilterInput | undefined;
    @Field(() => [ProductFilterInputFilterInput!], { nullable: true })
    AND?: ProductFilterInputFilterInput[] | undefined;
    @Field(() => [ProductFilterInputFilterInput!], { nullable: true })
    OR?: ProductFilterInputFilterInput[] | undefined;
}

@InputType()
export class ReviewFilterInputFilterInput {
    @Field(() => DateTimeFilterInput, { nullable: true })
    createdAt?: DateTimeFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    title?: StringFilterInput | undefined;
    @Field(() => StringFilterInput, { nullable: true })
    rating?: StringFilterInput | undefined;
    @Field(() => BooleanFilterInput, { nullable: true })
    verified?: BooleanFilterInput | undefined;
    @Field(() => [ReviewFilterInputFilterInput!], { nullable: true })
    AND?: ReviewFilterInputFilterInput[] | undefined;
    @Field(() => [ReviewFilterInputFilterInput!], { nullable: true })
    OR?: ReviewFilterInputFilterInput[] | undefined;
}

@InputType()
export class TagFilterInputFilterInput {
    @Field(() => StringFilterInput, { nullable: true })
    name?: StringFilterInput | undefined;
    @Field(() => [TagFilterInputFilterInput!], { nullable: true })
    AND?: TagFilterInputFilterInput[] | undefined;
    @Field(() => [TagFilterInputFilterInput!], { nullable: true })
    OR?: TagFilterInputFilterInput[] | undefined;
}

@InputType({ description: "Create input for Product" })
export class ProductCreateInput {
    @Field(() => String!)
    name!: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => Float!)
    price!: number;
    @Field(() => JSON, { nullable: true })
    metadata?: any;
}

@InputType({ description: "Update input for Product" })
export class ProductUpdateInput {
    @Field(() => String)
    id?: string;
    @Field(() => DateTime)
    createdAt?: Date;
    @Field(() => DateTime)
    updatedAt?: Date;
    @Field(() => String)
    name?: string;
    @Field(() => String, { nullable: true })
    description?: string;
    @Field(() => Float)
    price?: number;
    @Field(() => JSON, { nullable: true })
    metadata?: any;
}

@InputType({ description: "Create input for Review" })
export class ReviewCreateInput {
    @Field(() => String!)
    title!: string;
    @Field(() => String!)
    content!: string;
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
    @Field(() => DateTime)
    createdAt?: Date;
    @Field(() => String)
    title?: string;
    @Field(() => String)
    content?: string;
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
    @Field(() => DateTime!)
    assignedAt!: Date;
}

@InputType({ description: "Update input for ProductTag" })
export class ProductTagUpdateInput {
    @Field(() => String)
    productId?: string;
    @Field(() => String)
    tagId?: string;
    @Field(() => DateTime)
    assignedAt?: Date;
}

@InputType()
export class ProductQueryArgs {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
    @Field(() => Boolean, { nullable: true })
    connection?: boolean | undefined;
}

@InputType()
export class ReviewQueryArgs {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
    @Field(() => Boolean, { nullable: true })
    connection?: boolean | undefined;
}

@InputType()
export class TagQueryArgs {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
    @Field(() => Boolean, { nullable: true })
    connection?: boolean | undefined;
}

@InputType()
export class ProductTagQueryArgs {
    @Field(() => Int, { nullable: true })
    first?: number | undefined;
    @Field(() => String, { nullable: true })
    after?: string | undefined;
    @Field(() => Int, { nullable: true })
    last?: number | undefined;
    @Field(() => String, { nullable: true })
    before?: string | undefined;
    @Field(() => Boolean, { nullable: true })
    connection?: boolean | undefined;
}
